{-# LANGUAGE FlexibleInstances #-}

module Process (handleProcess, Resource(..)) where

import Control.DeepSeq (deepseq)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (ToJSON(..), encode)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BU
import qualified Data.CaseInsensitive as CI
import Data.Char (isSpace, isAlphaNum, toUpper)
import Data.Foldable (foldl')
import Data.Maybe (maybeToList)
import Network.HTTP.Types (Header, parseQuery, status200, status500)
import Network.Wai (requestHeaders, responseLBS, lazyRequestBody)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)
import System.IO (Handle, hGetLine, hIsEOF)
import System.Posix.IO (createPipe, closeFd, fdToHandle)
import System.Process (proc, CreateProcess(..), createProcess, StdStream(..), waitForProcess)

import Media

data Resource = Resource { rPath :: FilePath
                         , rQuery :: String
                         , rDir :: FilePath } deriving Show

handleProcess request execPath resource mediaType args replyHeaders = do
  -- Create handles for the status code and headers.
  (statusIn, statusOut) <- createPipe
  (headersIn, headersOut) <- createPipe
  -- This was already successfully parsed in order to get this far.
  vars' <- getEnvironment
  let vars = [("RESOURCE", rPath resource)]
          ++ map ((,) "REPRESENTATION" . mediaTypeToFileName) (maybeToList mediaType)
          ++ queryEnvVars (rQuery resource)
          ++ headerEnvVars (requestHeaders request)
          ++ [("STATUS_FD", show statusOut), ("HEADERS_FD", show headersOut)]
  (Just hin, Just hout, _, ph) <- createProcess ((proc execPath args)
                                                   { cwd = Just (takeDirectory execPath)
                                                   , env = Just (foldl' addToAL' vars' vars)
                                                   , std_in = CreatePipe
                                                   , std_out = CreatePipe })
  BL.hPut hin =<< lazyRequestBody request
  out <- BL.hGetContents hout
  exitCode <- waitForProcess ph
  case exitCode of
    ExitFailure _ -> return (responseLBS status500 [] "Internal Server Error")
    ExitSuccess -> do
      ignore (closeFd statusOut)
      ignore (closeFd headersOut)
      status <- readStatus =<< fdToHandle statusIn
      processHeaders <- readHeaders =<< fdToHandle headersIn
      -- Override or merge response headers with any headers written out by the process.
      let headers = replyHeaders <> processHeaders
      return (headers `deepseq` responseLBS status headers out)
 where ignore a = void (try a :: IO (Either SomeException ()))
       readStatus h = do
         eof <- hIsEOF h
         if eof
           then return status200
           else do
             statusString <- hGetLine h
             return (toEnum (read statusString))
       addToAL' l (key, value) = (key, value) : delFromAL' l key
       delFromAL' l key = filter (\a -> (fst a) /= key) l

varName :: String -> String
varName = map varChar
 where varChar c | isAlphaNum c = toUpper c
                 | otherwise = '_'

queryEnvVars :: String -> [(String, String)]
queryEnvVars queryString =
  [ ("QUERY_STRING", queryString')
  , ("QUERY_JSON", BLU.toString (encode query))
  , ("QUERY_PARAMS", unwords (map (queryVarName . fst) query))
  ] ++ map queryVar query
 where queryString' = case queryString of
                        [] -> []
                        _ -> tail queryString
       query = parseQuery (BU.fromString queryString')
       queryVarName name = varName (BU.toString name)
       queryVar (name, value) = ("QUERY_PARAM_" ++ queryVarName name, maybe "" BU.toString value)

instance ToJSON BU.ByteString where
  toJSON = toJSON . BU.toString

-- TODO: Ensure that duplicate headers are combined for supported headers.
instance ToJSON (CI.CI BU.ByteString) where
  toJSON = toJSON . CI.original

-- Present the headers in various formats to the child process via environment variables.
headerEnvVars :: [Header] -> [(String, String)]
headerEnvVars headers =
    -- a list of header variable names
  [ ("HEADERS", unwords (map (headerVarName . fst) headers))
    -- a single variable containing the headers as a JSON string
  , ("HEADERS_JSON", BLU.toString (encode headers))
    -- a variable for each header, prefixed with HEADER_
  ] ++ map headerVar headers
 where headerVarName name = varName (BU.toString (CI.original name))
       headerVar (name, value) = ("HEADER_" ++ headerVarName name, BU.toString value)

-- Read headers from a file.
readHeaders :: Handle -> IO [Header]
readHeaders = readHeaders' []
 where readHeaders' headers h = do
         eof <- hIsEOF h
         if eof
           then return (reverse headers)
           else do
             headerLine <- B8.hGetLine h
             readHeaders' (header headerLine : headers) h
       header s =
         case B8.break (== ':') s of
           (_, "") -> error "Failed to parse header line."
           (name, value) -> (CI.mk name, trim (B8.tail value))
       trim = B8.dropWhile isSpace . fst . B8.spanEnd isSpace
