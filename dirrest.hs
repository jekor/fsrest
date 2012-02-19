import Control.Concurrent (forkIO)
import Data.ByteString.Lazy (readFile, hPut, hGetContents)
import Data.Maybe (fromJust, fromMaybe)
import Network (PortID(..))
import Network.CGI (CGI, liftIO, requestURI, requestMethod, outputFPS, output, outputError, outputMethodNotAllowed, getBodyFPS, setHeader)
import Network.CGI.Protocol (CGIResult(..))
import Network.SCGI (runSCGIConcurrent')
import Network.URI (URI(..))
import System.Directory (getPermissions, readable, executable)
import System.Environment (getArgs)
import System.FilePath (takeFileName, dropFileName, normalise, (</>))
import System.Posix.Files (fileExist, getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink)
import System.Process (readProcess, proc, CreateProcess(..), createProcess, StdStream(..))

import Prelude hiding (readFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir, port] -> do
      let portNum = (read port)::Integer
      runSCGIConcurrent' forkIO 10 (PortNumber (fromIntegral portNum)) $ handleRequest dir
    _     -> putStrLn "dirrest <directory> <port>"

output404 :: CGI CGIResult
output404 = outputError 404 "Resource Not Found" []

availableMethods :: FilePath -> IO [String]
availableMethods _ = do
  return []

resourceGetFile :: FilePath -> IO (Maybe FilePath)
resourceGetFile path = do
  -- TODO: Is there an Accept header?
  -- TODO: Implement 300 Multiple Choices if no GET file exists but others do.
  let getPath = path ++ "/GET"
  exists <- fileExist getPath
  if exists
     then return $ Just $ normalise getPath
     else return Nothing

resourcePostFile :: FilePath -> IO (Maybe FilePath)
resourcePostFile path = do
  -- TODO: Consider the POST Content-Type.
  let postPath = path ++ "/POST"
  exists <- fileExist postPath
  if exists
     then return $ Just $ normalise postPath
     else return Nothing

translate :: (Eq a) => [(a, a)] -> [a] -> [a]
translate sr = map (\s -> fromMaybe s $ lookup s sr)

resolveSymlink :: FilePath -> IO FilePath
resolveSymlink path = do
  status <- getSymbolicLinkStatus path
  if isSymbolicLink status
     then do path' <- readSymbolicLink path
             resolveSymlink $ dropFileName path </> path'
     else return path

getMimeType :: FilePath -> IO (Maybe String)
getMimeType path = do
  path' <- resolveSymlink path
  case takeFileName path' of
    "GET" -> return Nothing
    n     -> return $ Just $ translate [('.', '/')] n

handleRequest :: FilePath -> CGI CGIResult
handleRequest dir = do
  uri  <- requestURI
  -- TODO: Ensure that the path is within dir.
  let path = dir ++ "/" ++ uriPath uri
  exists <- liftIO $ fileExist path
  if exists
     then do
       method <- requestMethod
       case method of
         "GET" -> do
            getPath <- liftIO $ resourceGetFile path
            case getPath of
              Nothing -> outputMethodNotAllowed =<< liftIO (availableMethods path)
              Just p  -> do
                perms <- liftIO $ getPermissions p
                if readable perms
                   then if executable perms
                           -- TODO: Does readProcess exist in a ByteString version?
                           then output =<< liftIO (readProcess p [] "")
                           else
                             do mimeType <- liftIO (getMimeType p)
                                case mimeType of
                                  Nothing -> return ()
                                  Just mt -> setHeader "Content-Type" $ mt ++ (if take 4 mt == "text" then "; charset=UTF-8" else "")
                                outputFPS =<< liftIO (readFile p)
                   else outputMethodNotAllowed =<< liftIO (availableMethods path)
         "POST" -> do
            postPath <- liftIO $ resourcePostFile path
            case postPath of
              Nothing -> outputMethodNotAllowed =<< liftIO (availableMethods path)
              Just p  -> do
                perms <- liftIO $ getPermissions p
                if readable perms && executable perms
                   then
                     do body <- getBodyFPS
                        outputFPS =<< liftIO (do
                          let proc' = proc p []
                              proc'' = proc' {cwd = Just path
                                             ,std_in = CreatePipe
                                             ,std_out = CreatePipe
                                             ,std_err = CreatePipe}
                          (hin, hout, _, _) <- createProcess proc''
                          hPut (fromJust hin) body
                          hGetContents (fromJust hout))
                   else outputMethodNotAllowed =<< liftIO (availableMethods path)
         _     -> outputMethodNotAllowed =<< liftIO (availableMethods path)
     else output404
