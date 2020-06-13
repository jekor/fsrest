{-# LANGUAGE QuasiQuotes #-}

import Control.Exception (handle, SomeException, bracket)
import Control.Monad (filterM, liftM2, when)
import Data.Attoparsec.ByteString.Char8 hiding (match)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (isJust, fromJust, catMaybes)
import Network.Socket (socket, bind, listen, defaultProtocol, getAddrInfo, Family(..), SocketType(..), AddrInfo(..), SockAddr(SockAddrUnix), setSocketOption, SocketOption(ReusePort), shutdown, ShutdownCmd(ShutdownReceive), close)
import Network.HTTP.Types (status200, status300, status400, status404, status405, status406, status415, hAccept, Method)
import qualified Network.URI as URI
import Network.Wai (requestMethod, requestHeaders, requestHeaderHost, rawPathInfo, responseLBS, responseFile, responseStatus, responseHeaders)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import System.Console.Docopt (docopt, parseArgsOrExit, isPresent, argument, longOption, exitWithUsage, getArgOrExitWith, getArg)
import System.Directory (getPermissions, Permissions, readable, executable, getDirectoryContents, doesDirectoryExist, makeAbsolute, removeFile)
import System.Environment (getArgs)
import System.FilePath (normalise, (</>), dropTrailingPathSeparator, splitPath, joinPath, takeBaseName)
import System.Posix.Files (readSymbolicLink, setFileMode, fileMode, getFileStatus, unionFileModes, otherWriteMode, otherReadMode)

import Media
import Process

usage = [docopt|
fsrest version 0.7.0

Usage:
  fsrest [-h] (--address --port | --socket) <directory>

Serve resources from directory <directory>.

Options:
  -h, --help             show this help message
  -a, --address=address  IP address to listen on
  -p, --port=port        port number to listen on
  -s, --socket=socket    UNIX socket to listen on
  |]

main = do
  args <- parseArgsOrExit usage =<< getArgs
  when (args `isPresent` (longOption "help")) (exitWithUsage usage)
  dir' <- getArgOrExitWith usage args (argument "directory")
  dir <- makeAbsolute dir'
  bracket
    (case getArg args (longOption "socket") of
       Just path -> do
         s <- socket AF_UNIX Stream defaultProtocol
         bind s (SockAddrUnix path)
         -- Make the socket world writable, like a port.
         setFileMode path =<< ((unionFileModes otherReadMode . unionFileModes otherWriteMode . fileMode) <$> getFileStatus path)
         return s
       _ -> do
         address <- getArgOrExitWith usage args (longOption "address")
         port <- getArgOrExitWith usage args (longOption "port")
         s <- socket AF_INET Stream defaultProtocol
         setSocketOption s ReusePort 1
         addrs <- getAddrInfo Nothing (Just address) (Just port)
         bind s (addrAddress (head addrs))
         return s)
    (\ s -> do shutdown s ShutdownReceive
               close s
               case getArg args (longOption "socket") of
                 Just path -> removeFile path
                 _ -> return ())
    (\ s -> do listen s 5
               runSettingsSocket defaultSettings s (app dir))

app dir request reply = do
  case (URI.parseURIReference (BU.toString (rawPathInfo request)), parseHost =<< (requestHeaderHost request)) of
    (Just uri, Just host) -> do
      let path = dropTrailingPathSeparator (URI.uriPath uri)
          resource = Resource path (URI.uriQuery uri) ((dir </> host) <> path)
      case CI.mk (requestMethod request) of
        "OPTIONS" -> do
          opts <- options resource
          reply (if null opts
                   then notFound
                   else responseLBS status200 [allowHeader opts] "")
        method' -> do
          allowed <- methodAllowed method' resource
          if allowed
            then do
              response <- handleRequest request resource
              reply (if method' == "HEAD" then headResponse response else response)
            else do
              exists <- doesDirectoryExist (rDir resource)
              case method' of
                method'' | method'' `elem` ["GET", "HEAD"] -> reply notFound
                         | method'' == "DELETE" && not exists -> reply notFound
                         | otherwise -> do
                             opts <- options resource
                             reply (responseLBS status405 [allowHeader opts] "")
    _ -> reply (responseLBS status400 [] "Bad Request")
 where allowHeader opts = ("Allow", B.intercalate ", " opts)
       parseHost s = case parseOnly hostP s of
                       Left _ -> Nothing
                       Right host -> Just host
       hostP = (map toLower . BU.toString . B.intercalate ".")
           <$> takeWhile1 (\ c -> isAlpha_ascii c || isDigit c || c == '-') `sepBy` char '.'

notFound = responseLBS status404 [] "Not Found"

-- TODO Don't send Content-Length.
headResponse response = responseLBS (responseStatus response) (responseHeaders response) ""

handleRequest request resource = do
  case CI.mk (requestMethod request) of
    method' | method' `elem` ["GET", "HEAD"] -> do
      reps <- availableRepresentations (rDir resource)
      -- When the directory exists but there are no representations, we consider it missing.
      if null reps
        then return notFound
        else
          let bestReps = case lookup hAccept (requestHeaders request) of
                           Nothing -> reps
                           Just accept' -> best (mediaTypeMatches reps accept')
          in case bestReps of
            [] -> return (notAcceptable reps)
            [r] -> reply' method' =<< representation request resource r
            rs ->
              -- Disambiguate with a GET symlink if it exists. Return multiple choices if it doesn't.
              handle ((const $ return $ multipleChoices rs) :: SomeException -> _) $ do
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
        Nothing -> return $ responseLBS status415 [] "Invalid Content-Type"
        Just mediaType -> handleProcess request execPath resource (Just mediaType) [] []
    "PUT" -> do
      execPath <- fromJust `fmap` resourceExecutable "PUT" resource
      case contentType of
        Nothing -> return $ responseLBS status415 [] "Invalid Content-Type"
        Just mediaType -> handleProcess request execPath resource (Just mediaType) [takeBaseName (rDir resource)] []
    "DELETE" -> do
      execPath <- fromJust `fmap` resourceExecutable "DELETE" resource
      handleProcess request execPath resource Nothing [takeBaseName (rDir resource)] []
    _ -> error "Unimplemented but allowed method. This shouldn't happen."
 where contentType = parseMediaType =<< lookup (CI.mk "Content-Type") (requestHeaders request)
       repsResp status rs = responseLBS status [("Content-Type", "text/plain; charset=utf-8")]
                              (BL.fromChunks [B.intercalate "\n" (map printMediaType rs)])
       multipleChoices = repsResp status300
       notAcceptable = repsResp status406
       reply' method' response =
         case method' of
           "GET" -> return response
           "HEAD" -> return (headResponse response)
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

representation request resource mt = do
  let dir = rDir resource
      file = dir </> mediaTypeToFileName mt
      headers = [("Content-Type", B.concat [mtType mt, "/", mtSubType mt, "; charset=utf-8"])]
  perms' <- getPermissions file
  if executable perms'
    then handleProcess request file resource (Just mt) [] headers
    else return (responseFile status200 headers file Nothing)
