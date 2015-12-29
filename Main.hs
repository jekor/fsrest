import Control.Concurrent (forkIO)
import Control.DeepSeq (deepseq)
import Control.Exception (finally, handle, SomeException, try)
import Control.Monad (filterM, forever, void, liftM2)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import Data.List (find, elemIndex)
import Data.Maybe (isJust, fromJust)
import Network (accept)
import Network.Socket (socket, bind, listen, defaultProtocol, getAddrInfo, Socket(..), Family(..), SocketType(..), AddrInfo(..))
import Network.HTTP.Toolkit (inputStreamFromHandle, readRequest, Request(..), Response(..), BodyReader, sendBody)
import Network.HTTP.Toolkit.Header (sendHeader)
import Network.HTTP.Toolkit.Response (formatStatusLine)
import Network.HTTP.Types (status200, status300, status400, status404, status500, status405, status406, status415, hAccept, Method)
import qualified Network.URI as URI
import Numeric (showHex)
import System.Directory (getPermissions, Permissions, readable, executable, getDirectoryContents, doesFileExist, doesDirectoryExist, makeAbsolute)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (takeFileName, takeDirectory, normalise, (</>), dropTrailingPathSeparator, splitPath, joinPath)
import System.IO (Handle, hPutStrLn, stderr, hClose, hGetLine, hIsEOF, hPrint)
import System.Posix.Files (fileAccess, readSymbolicLink)
import System.Posix.IO (createPipe, closeFd, fdToHandle)
import System.Process (proc, CreateProcess(..), createProcess, StdStream(..), waitForProcess)

import Headers
import Media
import Negotiation

main = do
  args <- getArgs
  case args of
    [dir', address, port] -> do
      dir <- makeAbsolute dir'
      s <- listen' address port
      forever $ do
        (h, _, _) <- accept s
        void $ forkIO (finally (serveClient h dir) (hClose h))
    _ -> do
      program <- getProgName
      hPutStrLn stderr $ "Usage: " ++ program ++ " <directory> <address> <port>"
      exitWith (ExitFailure 1)

listen' :: String -> String -> IO Socket
listen' address port = do
  addrs <- getAddrInfo Nothing (Just address) (Just port)
  s <- socket AF_INET Stream defaultProtocol
  bind s $ addrAddress $ head addrs
  listen s 5 -- maximum number of queued connections '5' should be fine
  return s

serveClient :: Handle -> FilePath -> IO ()
serveClient h dir = do
  conn <- inputStreamFromHandle h
  forever $ readRequest True conn >>= \ request@(Request method url _ _) ->
    handle handleError $
      case URI.parseURIReference $ BU.toString url of
        Nothing -> reply $ Response status400 [] "Bad Request URI"
        Just uri -> do
          let dirname = dir ++ dropTrailingPathSeparator (URI.uriPath uri)
          case CI.mk method of
            "OPTIONS" -> do
              opts <- options dirname
              reply (if null opts
                       then notFound
                       else Response status200 [allowHeader opts] "")
            method' -> do
              allowed <- methodAllowed method' dirname
              if allowed
                then do
                  response <- handleRequest method' request dirname
                  if method == "HEAD"
                    then replyEmpty response
                    else reply response
                else if method' `elem` ["GET", "HEAD"]
                       -- While technically correct to say that the GET method is not allowed,
                       -- better to say that the resource is not found.
                       then reply notFound
                       else do
                         opts <- options dirname
                         reply (Response status405 [allowHeader opts] "")
 where allowHeader opts = ("Allow", B.intercalate ", " opts)
       sendToClient = B.hPutStr h
       reply (Response status headers body) = do
         sendHeader sendToClient (formatStatusLine status) (headers ++ [("Transfer-Encoding", "Chunked")])
         mapM_ (sendToClient . chunk) $ BL.toChunks body
         sendToClient (chunk "")
       chunk s = foldr B.append "" [BU.fromString $ showHex (B.length s) "", "\r\n", s, "\r\n"]
       replyEmpty (Response status headers _) =
         sendHeader sendToClient (formatStatusLine status) headers
       handleError :: SomeException -> IO ()
       handleError e = do
         hPrint stderr e
         reply serverError

notFound = Response status404 [] "Not Found"
serverError = Response status500 [] "Internal Server Error"

handleRequest :: CI Method -> Request BodyReader -> FilePath -> IO (Response BL.ByteString)
handleRequest method request dirname =
  case method of
    method' | method' `elem` ["GET", "HEAD"] -> do
      reps <- availableRepresentations dirname
      -- When the directory exists but there are no representations, we consider it missing.
      if null reps
        then return notFound
        else
          let acceptable = lookup hAccept (requestHeaders request) 
              bests = case acceptable of
                        Nothing -> reps
                        Just a  -> let bests' = best $ matches (map repContentType reps) a in
                                     filter (\r -> isJust $ find (== repContentType r) bests') reps
          in case bests of
            [] -> return (notAcceptable reps)
            [r] -> reply' method' =<< representation request dirname r
            rs ->
              -- Disambiguate with a GET symlink if it exists. Return multiple choices if it doesn't.
              handle ((const $ return $ multipleChoices rs) :: SomeException -> IO (Response BL.ByteString)) $ do
                link <- readSymbolicLink $ dirname </> "GET"
                case find ((== link) . repPath) rs of
                  Nothing -> return $ multipleChoices rs
                  Just r -> reply' method' =<< representation request dirname r
    "POST" -> do
      -- We already checked that there's a file to handle POST in
      -- `options`. This does unnecessary reads from the disk. We also
      -- don't correctly handle any race conditions.
      execPath <- fromJust `fmap` resourceExecutable "POST" dirname
      case (parseMediaType . B8.unpack) =<< lookup (CI.mk "Content-Type") (requestHeaders request) of
        Nothing -> return $ Response status415 [] "Invalid Content-Type"
        Just mediaType -> handleProcess request execPath [mediaTypeFileName mediaType]
    "PUT" -> do
      execPath <- fromJust `fmap` resourceExecutable "PUT" dirname
      case (parseMediaType . B8.unpack) =<< lookup (CI.mk "Content-Type") (requestHeaders request) of
        Nothing -> return $ Response status415 [] "Invalid Content-Type"
        Just mediaType -> handleProcess request execPath [dirname, mediaTypeFileName mediaType]
    "DELETE" -> do
      execPath <- fromJust `fmap` resourceExecutable method dirname
      handleProcess request execPath [dirname]
    _ -> error "Unimplemented but allowed method. This shouldn't happen."
 where repsResp status rs = Response status [("Content-Type", "text/plain; charset=utf-8")]
                              (BL.fromChunks [B.intercalate "\n" $ map repContentType rs])
       multipleChoices = repsResp status300
       notAcceptable = repsResp status406
       reply' method' response@(Response code headers _) =
         case method' of
           "GET" -> return response
           "HEAD" -> return (Response code headers "")
           _ -> undefined

handleProcess request execPath args = do
  -- Create handles for the status code and headers.
  (statusIn, statusOut) <- createPipe
  (headersIn, headersOut) <- createPipe
  -- TODO: Ensure that headers don't overwrite sensitive variables. Perhaps prefix them.
  let vars = headerEnvVars (requestHeaders request)
          ++ [("STATUS_FD", show statusOut), ("HEADERS_FD", show headersOut)]
  (Just hin, Just hout, _, ph) <- createProcess ((proc execPath args)
                                                   { cwd = Just (takeDirectory execPath)
                                                   , env = Just vars
                                                   , std_in = CreatePipe
                                                   , std_out = CreatePipe })
  sendBody (B.hPut hin) (requestBody request)
  out <- BL.hGetContents hout
  exitCode <- waitForProcess ph
  case exitCode of
    ExitFailure _ -> return serverError
    ExitSuccess -> do
      ignore (closeFd statusOut)
      ignore (closeFd headersOut)
      status <- readStatus =<< fdToHandle statusIn
      headers <- readHeaders =<< fdToHandle headersIn
      return (headers `deepseq` Response status headers out)
 where ignore a = void (try a :: IO (Either SomeException ()))
       readStatus h = do
         eof <- hIsEOF h
         if eof
           then return status200
           else do
             statusString <- hGetLine h
             return (toEnum (read statusString))

-- Determine which methods are allowed on a given resource.
options :: FilePath -> IO [Method]
options dirname = filterM (\ m -> methodAllowed (CI.mk m) dirname) ["GET", "HEAD", "POST", "PUT", "DELETE"]

methodAllowed :: CI Method -> FilePath -> IO Bool
methodAllowed method dirname =
  case method of
    method' | method' `elem` ["GET", "HEAD"] -> do
      canRead <- perms [readable] dirname
      reps <- if canRead
                then availableRepresentations dirname
                else return []
      return (canRead && not (null reps))
    method' | method' `elem` ["POST", "PUT"] ->
      isJust `fmap` resourceExecutable method dirname
    "DELETE" -> doesDirectoryExist dirname <&&> (isJust `fmap` resourceExecutable "DELETE" dirname)
    _ -> return False
 where (<&&>) = liftM2 (&&)

-- Return the executable file usable to carry out a given method on a
-- resource (or Nothing if we do not have a way to carry out that
-- method).
resourceExecutable :: CI Method -> FilePath -> IO (Maybe FilePath)
resourceExecutable method dirname =
  case method of
    "POST" -> executableFile (dirname </> "POST")
    "PUT" -> executableFile (parent dirname </> "PUT")
    "DELETE" -> executableFile (parent dirname </> "DELETE")
    _ -> return Nothing
 where executableFile path = do
         canExec <- perms [executable] path
         return (if canExec then Just (normalise path) else Nothing)
       parent = joinPath . init . splitPath

perms :: [Permissions -> Bool] -> FilePath -> IO Bool
perms fs path = handle (const (return False) :: SomeException -> IO Bool) $ do
  permissions <- getPermissions path
  return (all ($ permissions) fs)

-- TODO: Charset and Language
data Representation = Representation { repPath        :: FilePath
                                     , repContentType :: B.ByteString }
                      deriving (Show, Eq)

fileRepresentation :: FilePath -> IO Representation
fileRepresentation path = return Representation { repPath        = path
                                                , repContentType = BU.fromString $ tr1 [('.', '/')] $ takeFileName path }
 where tr1 :: (Eq a) => [(a, a)] -> [a] -> [a]
       tr1 _ [] = []
       tr1 replacements (x:xs) =
         case lookup x replacements of
           Nothing -> x : tr1 replacements xs
           Just replacement -> replacement : xs

availableRepresentations :: FilePath -> IO [Representation]
availableRepresentations dirname =
  mapM fileRepresentation =<< filterM usable =<< getDirectoryContents dirname
 where usable :: FilePath -> IO Bool
       usable p =
         -- Mime types look like "text/html" and "text/vnd.abc". On the
         -- filesystem they'll look like "text.html" and "text.vnd.abc". So to
         -- determine which files are possible representations, we just have to
         -- look for at least 1 dot. However, we need to ignore hidden files
         -- like ".gitignore" or we'll get confusing results. To do that, we'll
         -- just check for at least 1 dot beyond the first character.
         case elemIndex '.' p of
           Just i | i > 0 -> do
             exists <- doesFileExist (dirname </> p)
             if exists
               then fileAccess (dirname </> p) True False False
               else return False
           _ -> return False

representation :: Request BodyReader -> FilePath -> Representation -> IO (Response BL.ByteString)
representation request dirname r = do
  -- TODO: Only set charset on text types.
  let contentType = repContentType r `B.append` "; charset=utf-8"
      headers = [("Content-Type", contentType)]
  perms' <- getPermissions $ dirname </> repPath r
  if executable perms'
    then handleProcess request (dirname </> repPath r) []
    else Response status200 headers `fmap` BL.readFile (dirname </> repPath r)
