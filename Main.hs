import Control.Arrow ((***))
import Control.Concurrent (forkIO)
import Control.Exception (finally, handle, SomeException)
import Control.Monad (filterM, forever, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import Data.List (find, elemIndex)
import Data.Maybe (isJust, fromJust)
import Network (accept)
import Network.Socket (socket, bind, listen, defaultProtocol, getAddrInfo, Socket(..), Family(..), SocketType(..), AddrInfo(..))
import Network.HTTP.Toolkit (inputStreamFromHandle, readRequest, Request(..), Response(..), BodyReader, sendBody)
import Network.HTTP.Toolkit.Header (sendHeader)
import Network.HTTP.Toolkit.Response (formatStatusLine)
import Network.HTTP.Types (status200, status300, status404, status500, status405, status406, status415, hAccept, Method)
import qualified Network.URI as URI
import Numeric (showHex)
import System.Directory (getPermissions, Permissions, readable, executable, getDirectoryContents, doesFileExist, setCurrentDirectory, makeAbsolute)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (takeFileName, takeDirectory, normalise, (</>), dropTrailingPathSeparator, splitPath, joinPath)
import System.IO (Handle, hPutStrLn, stderr, hClose)
import System.Posix.Files (fileAccess, readSymbolicLink)
import System.Process (readProcessWithExitCode, proc, CreateProcess(..), createProcess, StdStream(..), waitForProcess)

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
    case URI.parseURIReference $ BU.toString url of
      Nothing -> reply $ Response status500 [] "Failed to parse request URI"
      Just uri -> do
        let dirname = dir ++ dropTrailingPathSeparator (URI.uriPath uri)
        opts <- options dirname
        case CI.mk method of
          "OPTIONS" -> reply (if null opts
                                then notFound
                                else Response status200 [("Allow", B.intercalate ", " opts)] "")
          method' | method' `elem` map CI.mk opts -> reply =<< handleRequest method' request dirname
                    -- While correct to say that the GET method is not allowed
                    -- for something that may be PUT, the more intuitive
                    -- response is a 404.
                  | null opts || opts == ["PUT"] -> reply notFound
                  | otherwise -> reply (Response status405 [("Allow", B.intercalate ", " opts)] "")
 where sendToClient = B.hPutStr h
       reply = sendLazyResponse sendToClient

notFound = Response status404 [] "Not Found"

handleRequest :: CI Method -> Request BodyReader -> FilePath -> IO (Response BL.ByteString)
handleRequest method request dirname =
  case method of
    "GET" -> do
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
            [r] -> representation dirname r
            rs ->
              -- Disambiguate with a GET symlink if it exists. Return multiple choices if it doesn't.
              handle ((const $ return $ multipleChoices rs) :: SomeException -> IO (Response BL.ByteString)) $ do
                link <- readSymbolicLink $ dirname </> "GET"
                case find ((== link) . repPath) rs of
                  Nothing -> return $ multipleChoices rs
                  Just r -> representation dirname r
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

handleProcess request execPath args = do
  (out, exitCode) <- do
    let vars = map (B8.unpack . CI.original *** B8.unpack) (requestHeaders request)
    (Just hin, Just hout, _, ph) <- createProcess ((proc execPath args)
                                                     { cwd = Just (takeDirectory execPath)
                                                     , env = Just vars
                                                     , std_in = CreatePipe
                                                     , std_out = CreatePipe })
    sendBody (B.hPut hin) (requestBody request)
    out' <- BL.hGetContents hout
    exitCode' <- waitForProcess ph
    return (out', exitCode')
  return (case exitCode of
            ExitSuccess   -> Response status200 [] out
            ExitFailure _ -> Response status500 [] "Internal Server Error")

-- Determine which methods are allowed on a given resource.
options :: FilePath -> IO [Method]
options dirname = do
  canRead <- perms [readable] dirname
  reps <- if canRead
            then availableRepresentations dirname
            else return []
  canPost <- isJust `fmap` resourceExecutable "POST" dirname
  canPut <- isJust `fmap` resourceExecutable "PUT" dirname
  canDelete <- isJust `fmap` resourceExecutable "DELETE" dirname
  return (["GET" | canRead && not (null reps)] ++
          ["POST" | canPost] ++ ["PUT" | canPut] ++ ["DELETE" | canDelete])

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

representation :: FilePath -> Representation -> IO (Response BL.ByteString)
representation dirname r = do
  let contentType = repContentType r `B.append` "; charset=utf-8"
      headers = [("Content-Type", contentType)]
  perms' <- getPermissions $ dirname </> repPath r
  if executable perms'
    then do
      setCurrentDirectory dirname
      -- TODO: Read the process output lazily as a bytestring.
      -- TODO: Support setting headers.
      (exit, out, err) <- readProcessWithExitCode (dirname </> repPath r) [] ""
      case exit of
        ExitSuccess -> return $ Response status200 headers (BLU.fromString out)
        _           -> return $ Response status500 [] (BLU.fromString err)
    else Response status200 headers `fmap` BL.readFile (dirname </> repPath r)

sendLazyResponse :: (B.ByteString -> IO ()) -> Response BL.ByteString -> IO ()
sendLazyResponse send (Response status headers body) = do
  sendHeader send (formatStatusLine status) headers'
  mapM_ (send . chunk) $ BL.toChunks body
  send $ chunk ""
 where headers' = ("Transfer-Encoding", "Chunked") : headers
       chunk :: B.ByteString -> B.ByteString
       chunk s = foldr B.append "" [BU.fromString $ showHex (B.length s) "", "\r\n", s, "\r\n"]
