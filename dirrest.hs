import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Data.ByteString.Lazy (readFile, hPut, hGetContents)
import Data.Maybe (fromJust)
import Network (PortID(..))
import Network.CGI (CGI, liftIO, requestURI, requestMethod, outputFPS, output, outputError, outputMethodNotAllowed, getBodyFPS)
import Network.CGI.Protocol (CGIResult(..))
import Network.SCGI (runSCGIConcurrent')
import Network.URI (URI(..))
import System.Directory (getPermissions, readable, executable)
import System.Environment (getArgs)
import System.Posix.Files (fileExist)
import System.Process (readProcess, proc, CreateProcess(..), createProcess, StdStream(..))

import Prelude hiding (readFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] -> runSCGIConcurrent' forkIO 10 (PortNumber 10040) $ handleRequest dir
    _     -> putStrLn "dirrest <directory>"

output404 :: CGI CGIResult
output404 = outputError 404 "Resource Not Found" []

availableMethods :: FilePath -> IO [String]
availableMethods path = do
  return []

resourceGetFile :: FilePath -> IO (Maybe FilePath)
resourceGetFile path = do
  -- TODO: Is there an Accept header?
  -- TODO: Implement 300 Multiple Choices if no GET file exists but others do.
  let getPath = path ++ "/GET"
  exists <- fileExist getPath
  if exists
     then return $ Just getPath
     else return Nothing

resourcePostFile :: FilePath -> IO (Maybe FilePath)
resourcePostFile path = do
  -- TODO: Consider the POST Content-Type.
  let postPath = path ++ "/POST"
  exists <- fileExist postPath
  if exists
     then return $ Just postPath
     else return Nothing

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
                           else outputFPS =<< liftIO (readFile p)
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
                          (hin, hout, herr, proc''') <- createProcess proc''
                          hPut (fromJust hin) body
                          hGetContents (fromJust hout))
                   else outputMethodNotAllowed =<< liftIO (availableMethods path)
         _     -> outputMethodNotAllowed =<< liftIO (availableMethods path)
     else outputError 404 "Resource Not Found" [path]
