-- Copyright 2012, 2013, 2014 Chris Forno

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
import Data.List (find)
import Data.Maybe (isJust)
import Network (accept)
import Network.Socket (socket, bind, listen, defaultProtocol, getAddrInfo, Socket(..), Family(..), SocketType(..), AddrInfo(..))
import Network.HTTP.Toolkit (inputStreamFromHandle, readRequest, Request(..), Response(..), BodyReader, sendBody)
import Network.HTTP.Toolkit.Header (sendHeader)
import Network.HTTP.Toolkit.Response (formatStatusLine)
import Network.HTTP.Types (status200, status300, status404, status500, hAccept)
import qualified Network.URI as URI
import Numeric (showHex)
import System.Directory (doesDirectoryExist, getPermissions, readable, executable, getDirectoryContents, doesFileExist, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, normalise, (</>), dropTrailingPathSeparator)
import System.IO (Handle, hPutStrLn, stderr, hClose)
import System.Posix.Files (fileExist, fileAccess, readSymbolicLink)
import System.Process (readProcessWithExitCode, proc, CreateProcess(..), createProcess, StdStream(..), waitForProcess)

import Negotiation

main = do
  args <- getArgs
  case args of
    [dir, address, port] -> do
      s <- listen' address port
      forever $ do
        (h, _, _) <- accept s
        void $ forkIO (finally (serveClient h dir) (hClose h))
    _ -> do
      program <- getProgName
      hPutStrLn stderr $ program ++ " <directory> <address> <port>"

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
        exists <- doesDirectoryExist dirname
        if exists
          then do
            perms <- getPermissions dirname
            if readable perms
              then case method of
                     "GET"  -> reply =<< handleGet request dirname
                     "POST" -> reply =<< handlePost request dirname
                     _ -> reply notFound
              else reply notFound
          else reply notFound
 where sendToClient = B.hPutStr h
       reply = sendLazyResponse sendToClient

notFound = Response status404 [] "Not Found"

handleGet :: Request a -> FilePath -> IO (Response BL.ByteString)
handleGet request dirname = do
  reps <- availableRepresentations dirname
  let acceptable = lookup hAccept (requestHeaders request) 
      bests = case acceptable of
                Nothing -> reps
                Just a  -> let bests' = best $ matches (map repContentType reps) a in
                             filter (\r -> isJust $ find (== repContentType r) bests') reps
  case bests of
    [] -> return notFound
    [r] -> representation dirname r
    rs ->
      -- Disambiguate with a GET symlink if it exists. Return multiple choices if it doesn't.
      handle ((const $ return $ multipleChoices rs) :: SomeException -> IO (Response BL.ByteString)) $ do
        link <- readSymbolicLink $ dirname </> "GET"
        case find ((== link) . repPath) rs of
          Nothing -> return $ multipleChoices rs
          Just r -> representation dirname r

handlePost :: Request BodyReader -> FilePath -> IO (Response BL.ByteString)
handlePost (Request _ _ headers body) dirname = do
  postPath <- resourcePostFile dirname
  case postPath of
    Nothing -> return notFound
    Just p  -> do
      perms <- getPermissions p
      if readable perms && executable perms
        then do
          (out, exitCode) <- do
            let proc' = (proc p []) { cwd = Just dirname
                                    , env = Just (map (B8.unpack . CI.original *** B8.unpack) headers)
                                    , std_in = CreatePipe
                                    , std_out = CreatePipe }
            (Just hin, Just hout, _, ph) <- createProcess proc'
            sendBody (B.hPut hin) body
            out' <- BL.hGetContents hout
            exitCode' <- waitForProcess ph
            return (out', exitCode')
          case exitCode of
            ExitSuccess   -> return $ Response status200 [] out
            ExitFailure _ -> return $ Response status500 [] "Internal Server Error"
        else return notFound

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
       usable p = do
         exists <- doesFileExist (dirname </> p)
         if exists
            then fileAccess (dirname </> p) True False False
            else return False

resourcePostFile :: FilePath -> IO (Maybe FilePath)
resourcePostFile dirname = do
  -- TODO: Consider the POST Content-Type.
  let path = dirname </> "POST"
  exists <- fileExist path
  return (if exists then Just $ normalise path else Nothing)

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

multipleChoices :: [Representation] -> Response BL.ByteString
multipleChoices rs = Response status300 [] (BL.fromChunks [B.intercalate "\n" $ map repContentType rs])

sendLazyResponse :: (B.ByteString -> IO ()) -> Response BL.ByteString -> IO ()
sendLazyResponse send (Response status headers body) = do
  sendHeader send (formatStatusLine status) headers'
  mapM_ (send . chunk) $ BL.toChunks body
  send $ chunk ""
 where headers' = ("Transfer-Encoding", "Chunked") : headers
       chunk :: B.ByteString -> B.ByteString
       chunk s = foldr B.append "" [BU.fromString $ showHex (B.length s) "", "\r\n", s, "\r\n"]
