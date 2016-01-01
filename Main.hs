import Control.Concurrent (forkIO)
import Control.Exception (finally, handle, SomeException)
import Control.Monad (filterM, forever, void, liftM2)
import Data.Attoparsec.ByteString.Char8 hiding (match)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (isJust, fromJust, catMaybes)
import Network (accept)
import Network.Socket (socket, bind, listen, defaultProtocol, getAddrInfo, Socket(..), Family(..), SocketType(..), AddrInfo(..))
import Network.HTTP.Toolkit (inputStreamFromHandle, readRequest, Request(..), Response(..), BodyReader)
import Network.HTTP.Toolkit.Header (sendHeader)
import Network.HTTP.Toolkit.Response (formatStatusLine)
import Network.HTTP.Types (status200, status300, status400, status404, status500, status405, status406, status415, hAccept, Method)
import qualified Network.URI as URI
import Numeric (showHex)
import System.Directory (getPermissions, Permissions, readable, executable, getDirectoryContents, doesDirectoryExist, makeAbsolute)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (normalise, (</>), dropTrailingPathSeparator, splitPath, joinPath, takeBaseName)
import System.IO (Handle, hPutStrLn, stderr, hClose, hPrint)
import System.Posix.Files (readSymbolicLink)

import Media
import Process

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
  forever $ readRequest True conn >>= \ request@(Request method url headers _) ->
    handle handleError $
      case (URI.parseURIReference (BU.toString url), parseHost =<< lookup "Host" headers) of
        (Just uri, Just host) -> do
          let path = dropTrailingPathSeparator (URI.uriPath uri)
              resource = Resource path (URI.uriQuery uri) ((dir </> host) ++ path)
          case CI.mk method of
            "OPTIONS" -> do
              opts <- options resource
              reply (if null opts
                       then notFound
                       else Response status200 [allowHeader opts] "")
            method' -> do
              allowed <- methodAllowed method' resource
              if allowed
                then do
                  response <- handleRequest request resource
                  if method == "HEAD"
                    then replyEmpty response
                    else reply response
                else do
                  exists <- doesDirectoryExist (rDir resource)
                  case method' of
                    method'' | method'' `elem` ["GET", "HEAD"] -> reply notFound
                             | method'' == "DELETE" && not exists -> reply notFound
                             | otherwise -> do
                                 opts <- options resource
                                 reply (Response status405 [allowHeader opts] "")
        _ -> reply (Response status400 [] "Bad Request")
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
         reply (Response status500 [] "Internal Server Error")
       parseHost s = case parseOnly hostP s of
                       Left _ -> Nothing
                       Right host -> Just host
       hostP = (map toLower . BU.toString . B.intercalate ".")
           <$> takeWhile1 (\ c -> isAlpha_ascii c || isDigit c || c == '-') `sepBy` char '.'

notFound = Response status404 [] "Not Found"

handleRequest :: Request BodyReader -> Resource -> IO (Response BL.ByteString)
handleRequest request@(Request method _ headers _) resource =
  case CI.mk method of
    method' | method' `elem` ["GET", "HEAD"] -> do
      reps <- availableRepresentations (rDir resource)
      -- When the directory exists but there are no representations, we consider it missing.
      if null reps
        then return notFound
        else
          let bestReps = case lookup hAccept headers of
                           Nothing -> reps
                           Just accept' -> best (mediaTypeMatches reps accept')
          in case bestReps of
            [] -> return (notAcceptable reps)
            [r] -> reply' method' =<< representation request resource r
            rs ->
              -- Disambiguate with a GET symlink if it exists. Return multiple choices if it doesn't.
              handle ((const $ return $ multipleChoices rs) :: SomeException -> IO (Response BL.ByteString)) $ do
                link <- readSymbolicLink (rDir resource </> "GET")
                case find ((== link) . mediaTypeToFileName) rs of
                  Nothing -> return $ multipleChoices rs
                  Just r -> reply' method' =<< representation request resource r
    "POST" -> do
      -- We already checked that there's a file to handle POST in
      -- `options`. This does unnecessary reads from the disk. We also
      -- don't correctly handle any race conditions.
      execPath <- fromJust `fmap` resourceExecutable "POST" resource
      case contentType of
        Nothing -> return $ Response status415 [] "Invalid Content-Type"
        Just mediaType -> handleProcess request execPath resource (Just mediaType) [] []
    "PUT" -> do
      execPath <- fromJust `fmap` resourceExecutable "PUT" resource
      case contentType of
        Nothing -> return $ Response status415 [] "Invalid Content-Type"
        Just mediaType -> handleProcess request execPath resource (Just mediaType) [takeBaseName (rDir resource)] []
    "DELETE" -> do
      execPath <- fromJust `fmap` resourceExecutable "DELETE" resource
      handleProcess request execPath resource Nothing [takeBaseName (rDir resource)] []
    _ -> error "Unimplemented but allowed method. This shouldn't happen."
 where contentType = parseMediaType =<< lookup (CI.mk "Content-Type") headers
       repsResp status rs = Response status [("Content-Type", "text/plain; charset=utf-8")]
                              (BL.fromChunks [B.intercalate "\n" (map printMediaType rs)])
       multipleChoices = repsResp status300
       notAcceptable = repsResp status406
       reply' method' response@(Response code headers' _) =
         case method' of
           "GET" -> return response
           "HEAD" -> return (Response code headers' "")
           _ -> undefined

-- Determine which methods are allowed on a given resource.
options :: Resource -> IO [Method]
options resource = filterM (\ m -> methodAllowed (CI.mk m) resource) ["GET", "HEAD", "POST", "PUT", "DELETE"]

methodAllowed :: CI Method -> Resource -> IO Bool
methodAllowed method resource =
  let dir = rDir resource in
  case method of
    method' | method' `elem` ["GET", "HEAD"] -> do
      canRead <- perms [readable] dir
      reps <- if canRead
                then availableRepresentations dir
                else return []
      return (canRead && not (null reps))
    -- PUT and DELETE on the root resource don't make sense because
    -- looking for PUT or DELETE executables in the parent directory
    -- is out of our scope.
    method' | method' `elem` ["PUT", "DELETE"] && (rPath resource == "/") -> return False
    method' | method' `elem` ["POST", "PUT"] ->
      isJust `fmap` resourceExecutable method resource
    "DELETE" -> doesDirectoryExist (rDir resource) <&&> (isJust `fmap` resourceExecutable "DELETE" resource)
    _ -> return False
 where (<&&>) = liftM2 (&&)

-- Return the executable file usable to carry out a given method on a
-- resource (or Nothing if we do not have a way to carry out that
-- method).
resourceExecutable :: CI Method -> Resource -> IO (Maybe FilePath)
resourceExecutable method resource =
  let dir = rDir resource in
  case method of
    "POST" -> executableFile (dir </> "POST")
    "PUT" -> executableFile (parent dir </> "PUT")
    "DELETE" -> executableFile (parent dir </> "DELETE")
    _ -> return Nothing
 where executableFile path = do
         canExec <- perms [executable] path
         return (if canExec then Just (normalise path) else Nothing)
       parent = joinPath . init . splitPath

perms :: [Permissions -> Bool] -> FilePath -> IO Bool
perms fs path = handle (const (return False) :: SomeException -> IO Bool) $ do
  permissions <- getPermissions path
  return (all ($ permissions) fs)

availableRepresentations :: FilePath -> IO [MediaType]
availableRepresentations dir =
  (return . catMaybes) =<< mapM usable =<< getDirectoryContents dir
 where usable p = case mediaTypeFromFileName p of
                    Nothing -> return Nothing
                    Just mt -> do
                      canRead <- perms [readable] (dir </> p)
                      return (if canRead then Just mt else Nothing)

representation :: Request BodyReader -> Resource -> MediaType -> IO (Response BL.ByteString)
representation request resource mt = do
  let dir = rDir resource
      file = dir </> mediaTypeToFileName mt
      headers = [("Content-Type", B.concat [mtType mt, "/", mtSubType mt, "; charset=utf-8"])]
  perms' <- getPermissions file
  if executable perms'
    then handleProcess request file resource (Just mt) [] headers
    else Response status200 headers `fmap` BL.readFile file
