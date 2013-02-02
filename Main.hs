import Control.Concurrent (forkIO)
import Control.Monad (filterM, forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as UTF8L
import Data.List (find)
import Data.Maybe (fromMaybe, isJust)
import Network (listenOn, accept, PortID(..))
import System.Directory (doesDirectoryExist, getPermissions, readable, executable, getDirectoryContents, doesFileExist, setCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, normalise, (</>))
import System.IO (hPutStrLn, stderr, hClose)
import System.IO.Error (tryIOError)
import System.Posix.Files (fileExist, fileAccess, readSymbolicLink)
import System.Process (readProcess, proc, CreateProcess(..), createProcess, StdStream(..), waitForProcess)

import SCGI (SCGI, Body, Response(..))
import qualified SCGI
import Negotiation

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir, port] -> do
      let portNum = (read port)::Integer
      socket <- listenOn (PortNumber (fromIntegral portNum))
      forever $ do
        (handle, _, _) <- accept socket
        _ <- forkIO (tryIOError (SCGI.runRequest handle $ handleRequest dir) >> hClose handle)
        return ()
    _     -> hPutStrLn stderr "fsrest <directory> <port>"

handleRequest :: FilePath -> Body -> SCGI Response
handleRequest dir body = do
  uri <- SCGI.header "SCRIPT_NAME"
  case uri of
    Nothing -> output404
    Just uri' -> do
      -- TODO: Ensure that the path is within dir.
      path' <- SCGI.header "PATH_INFO"
      let path = dir ++ UTF8.toString (uri' `B.append` fromMaybe "" path')
      exists <- liftIO $ doesDirectoryExist path
      if exists
        then do
          liftIO $ setCurrentDirectory path
          perms <- liftIO $ getPermissions path
          if readable perms
             then do
               method <- SCGI.method
               case method of
                 Just "GET" -> handleGet path
                 Just "POST" -> handlePost path body
                 _ -> output404
                 -- TODO: Support 405 Method Not Allowed.
             else return $ Response "403 Forbidden" ""
        else output404

handleGet :: FilePath -> SCGI Response
handleGet path = do
  accept' <- SCGI.header "HTTP_ACCEPT"
  reps <- liftIO $ availableRepresentations path
  let bests = case accept' of
                Nothing -> reps
                Just a  -> let bests' = bestMatches (map repContentType reps) a in
                             filter (\r -> isJust $ find (== repContentType r) bests') reps
  case bests of
    [] -> output404
    [r] -> outputRepresentation r
    rs -> do
      -- Disambiguate with a GET symlink if it exists.
      link <- liftIO $ tryIOError $ readSymbolicLink $ path </> "GET"
      case link of
        Right path' -> case find ((== path') . repPath) rs of
                         Nothing -> outputMultipleChoices rs
                         Just r -> outputRepresentation r
        _ -> outputMultipleChoices rs

handlePost :: FilePath -> Body -> SCGI Response
handlePost path body = do
  postPath <- liftIO $ resourcePostFile path
  case postPath of
    Nothing -> output404
    Just p  -> do
      perms <- liftIO $ getPermissions p
      if readable perms && executable perms
        then do
          vars <- SCGI.allHeaders
          -- contentType <- SCGI.header "Content-Type"
          (out, exitCode) <- liftIO $ do
            let proc' = (proc p []) { cwd = Just path
                                    , env = Just (map (\(a, b) -> (B8.unpack a, B8.unpack b)) vars) -- (vars ++ maybe [] (\t -> [("CONTENT_TYPE", t), ("FSREST_CONTENT_TYPE", translate [('/', '.')] t)]) contentType)
                                    , std_in = CreatePipe
                                    , std_out = CreatePipe }
            (Just hin, Just hout, _, ph) <- createProcess proc'
            BL.hPut hin body
            out' <- BL.hGetContents hout
            exitCode' <- waitForProcess ph
            return (out', exitCode')
          case exitCode of
            ExitSuccess   -> return $ Response "200 OK" out
            ExitFailure _ -> return $ Response "500 Internal Server Error" out
        else output404

translate :: (Eq a) => [(a, a)] -> [a] -> [a]
translate sr = map (\s -> fromMaybe s $ lookup s sr)

output404 :: SCGI Response
output404 = return $ Response "404 Resource Not Found" ""

-- TODO: Charset and Language
data Representation = Representation { repPath        :: FilePath
                                     , repContentType :: B.ByteString }
                      deriving (Show, Eq)

-- TODO: Support mime types with dots in them (only replace the first one).
fileRepresentation :: FilePath -> IO Representation
fileRepresentation path = return Representation { repPath        = path
                                                , repContentType = UTF8.fromString $ translate [('.', '/')] $ takeFileName path }

availableRepresentations :: FilePath -> IO [Representation]
availableRepresentations path =
  mapM fileRepresentation =<< filterM usable =<< getDirectoryContents path
 where usable :: FilePath -> IO Bool
       usable p = do
         exists <- doesFileExist p
         if exists
            then fileAccess p True False False
            else return False

resourcePostFile :: FilePath -> IO (Maybe FilePath)
resourcePostFile path = do
  -- TODO: Consider the POST Content-Type.
  let postPath = path ++ "/POST"
  exists <- fileExist postPath
  if exists
     then return $ Just $ normalise postPath
     else return Nothing

outputRepresentation :: Representation -> SCGI Response
outputRepresentation r = do
  perms' <- liftIO $ getPermissions $ repPath r
  if executable perms'
    -- TODO: Need to support passing a status back.
    then return . Response "200 OK" . UTF8L.fromString =<< liftIO (readProcess (repPath r) [] "")
    else do
      SCGI.setHeader "Content-Type" $ repContentType r `B.append` "; charset=utf-8"
      return . Response "200 OK" =<< liftIO (BL.readFile $ repPath r)

outputMultipleChoices :: [Representation] -> SCGI Response
outputMultipleChoices rs = return $ Response "300 Multiple Choices" $ BL.fromChunks [B.intercalate "\n" $ map repContentType rs]
