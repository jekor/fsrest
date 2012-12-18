import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Monad (filterM)
import Data.ByteString.Lazy (readFile, hPut, hGetContents)
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe, catMaybes, maybeToList)
import Network (PortID(..))
-- Warning: This uses a modified Network.CGI. I couldn't find a way to just
-- override the functions I needed because Network.CGI hides the internals that
-- need to be referenced.
import Network.CGI (CGI, liftIO, requestURI, requestMethod, outputFPS, output, outputError, outputMethodNotAllowed, getBodyFPS, getInputs, negotiate, Accept, setHeader, requestAccept, requestContentType, parseContentType, showContentType, ContentType(..), Charset(..), Language, setStatus)
import Network.CGI.Protocol (CGIResult(..))
import Network.SCGI (runSCGIConcurrent')
import Network.URI (URI(..))
import System.Directory (doesDirectoryExist, getPermissions, readable, executable, getDirectoryContents, doesFileExist, setCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, normalise, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)
import System.Posix.Files (fileExist, fileAccess, readSymbolicLink)
import System.Process (readProcess, proc, CreateProcess(..), createProcess, StdStream(..), waitForProcess)

import Prelude hiding (readFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir, port] -> do
      let portNum = (read port)::Integer
      runSCGIConcurrent' forkIO 10 (PortNumber (fromIntegral portNum)) $ handleRequest dir
    _     -> hPutStrLn stderr "fsrest <directory> <port>"

translate :: (Eq a) => [(a, a)] -> [a] -> [a]
translate sr = map (\s -> fromMaybe s $ lookup s sr)

output404 :: CGI CGIResult
output404 = outputError 404 "Resource Not Found" []

availableMethods :: FilePath -> IO [String]
availableMethods _ = do
  return []

data Representation = Representation { repPath        :: FilePath
                                     , repContentType :: ContentType
                                     , repCharset     :: Maybe Charset
                                     , repLanguage    :: Maybe Language }
                      deriving (Show, Eq)

fileRepresentation :: FilePath -> IO (Maybe Representation)
fileRepresentation path = do
  -- TODO: Support mime types with dots in them (only replace the first one).
  let ct' = parseContentType $ translate [('.', '/')] $ takeFileName path
  case ct' of
    Nothing -> return Nothing
    Just ct -> return $ Just $ Representation { repPath        = path
                                              -- Default to UTF-8 for text for now.
                                              , repContentType = if ctType ct == "text" then ct {ctParameters = [("charset", "UTF-8")]} else ct
                                              , repCharset     = if ctType ct == "text" then Just (Charset "UTF-8") else Nothing
                                              , repLanguage    = Nothing }

availableRepresentations :: FilePath -> IO [Representation]
availableRepresentations path =
  catMaybes <$> (mapM fileRepresentation =<< filterM usable =<< getDirectoryContents path)
 where usable :: FilePath -> IO Bool
       usable p = do
         exists <- doesFileExist p
         if exists
            then fileAccess p True False False
            else return False

resourceGet :: FilePath -> Maybe (Accept ContentType) -> IO [Representation]
resourceGet path accept = do
  reps <- availableRepresentations path
  -- For now, only negotiate the content type.
  case negotiate (map repContentType reps) accept of
    []     -> return []
    ct:cts -> do
      -- Is the quality of the first representation unsurpassed?
      case filter ((== (snd ct)) . snd) cts of
        [] -> return $ maybeToList $ find ((== (fst ct)) . repContentType) reps
        cts' -> return $ filter ((`elem` (map fst (ct:cts'))) . repContentType) reps

resourcePostFile :: FilePath -> IO (Maybe FilePath)
resourcePostFile path = do
  -- TODO: Consider the POST Content-Type.
  let postPath = path ++ "/POST"
  exists <- fileExist postPath
  if exists
     then return $ Just $ normalise postPath
     else return Nothing

outputRepresentation :: Representation -> CGI CGIResult
outputRepresentation r = do
  perms' <- liftIO $ getPermissions $ repPath r
  if executable perms'
    then output =<< liftIO (readProcess (repPath r) [] "")
    else do
      setHeader "Content-Type" $ showContentType $ repContentType r
      outputFPS =<< liftIO (readFile $ repPath r)

outputMultipleChoices :: [Representation] -> CGI CGIResult
outputMultipleChoices rs = do
  setStatus 300 "Multiple Choices"
  output $ intercalate "\n" $ map (showContentType . repContentType) rs

handleRequest :: FilePath -> CGI CGIResult
handleRequest dir = do
  uri  <- requestURI
  -- TODO: Ensure that the path is within dir.
  let path = dir ++ "/" ++ uriPath uri
  exists <- liftIO $ doesDirectoryExist path
  if exists
     then do
       liftIO $ setCurrentDirectory path
       perms <- liftIO $ getPermissions path
       if readable perms
          then do
            method <- requestMethod
            case method of
              "GET" -> do
                accept <- requestAccept
                reps <- liftIO $ resourceGet path accept
                case reps of
                  -- TODO: outputMethodNotAllowed should only happen if there are other available methods. If not, it should be a 404.
                  [] -> outputMethodNotAllowed =<< liftIO (availableMethods path)
                  [r] -> outputRepresentation r
                  rs -> do
                    -- Disambiguate with a GET symlink if it exists.
                    link <- liftIO $ tryIOError $ readSymbolicLink $ path </> "GET"
                    case link of
                      Right path' -> case find ((== path') . repPath) rs of
                                       Nothing -> outputMultipleChoices rs
                                       Just r -> outputRepresentation r
                      _ -> outputMultipleChoices rs
              "POST" -> do
                postPath <- liftIO $ resourcePostFile path
                case postPath of
                  Nothing -> outputMethodNotAllowed =<< liftIO (availableMethods path)
                  Just p  -> do
                    perms' <- liftIO $ getPermissions p
                    if readable perms && executable perms'
                       then
                         do body <- getBodyFPS
                            inputs <- getInputs
                            contentType <- requestContentType
                            (out, exitCode) <- liftIO (do
                              let proc' = (proc p []) {cwd = Just path
                                                      ,env = Just (inputs ++ maybe [] (\t -> [("CONTENT_TYPE", t), ("FSREST_CONTENT_TYPE", translate [('/', '.')] t)]) contentType)
                                                      ,std_in = CreatePipe
                                                      ,std_out = CreatePipe}
                              (Just hin, Just hout, _, ph) <- createProcess proc'
                              hPut hin body
                              out' <- hGetContents hout
                              exitCode' <- waitForProcess ph
                              return (out', exitCode'))
                            case exitCode of
                              ExitSuccess   -> setStatus 200 "OK" >> outputFPS out
                              ExitFailure _ -> setStatus 400 "Bad Request" >> outputFPS out
                       else outputMethodNotAllowed =<< liftIO (availableMethods path)
              _     -> outputMethodNotAllowed =<< liftIO (availableMethods path)
          else outputError 403 "Forbidden" []
     else output404
