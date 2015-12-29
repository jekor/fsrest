{-# LANGUAGE FlexibleInstances #-}

module Process (handleProcess) where

import Control.DeepSeq (deepseq)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (ToJSON(..), object, (.=), encode)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BU
import qualified Data.CaseInsensitive as CI
import Data.Char (isSpace, isAlphaNum, toUpper)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Toolkit (Request(..), Response(..), BodyReader, sendBody)
import Network.HTTP.Types (Header, HeaderName, status200, status500)
import System.Exit (ExitCode(..))
import System.FilePath (takeDirectory)
import System.IO (Handle, hGetLine, hIsEOF)
import System.Posix.IO (createPipe, closeFd, fdToHandle)
import System.Process (proc, CreateProcess(..), createProcess, StdStream(..), waitForProcess)

handleProcess :: Request BodyReader -> FilePath -> [String] -> IO (Response BL.ByteString)
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
  sendBody (B8.hPut hin) (requestBody request)
  out <- BL.hGetContents hout
  exitCode <- waitForProcess ph
  case exitCode of
    ExitFailure _ -> return (Response status500 [] "Internal Server Error")
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

varName :: String -> String
varName = map varChar
 where varChar c | isAlphaNum c = toUpper c
                 | otherwise = '_'

-- TODO: Ensure that duplicate headers are combined for supported headers.
instance ToJSON [Header] where
  toJSON = object . map header
   where header (name, value) = decodeUtf8 (CI.original name) .= decodeUtf8 value

-- Present the headers in various formats to the child process via environment variables.
headerEnvVars :: [Header] -> [(String, String)]
headerEnvVars headers =
    -- a list of header variable names
  [ ("HEADERS", unwords (map (headerVarName . fst) headers))
    -- a single variable containing the headers as a JSON string
  , ("HEADERS_JSON", BLU.toString (encode headers))
    -- a variable for each header, prefixed with HEADER_
  ] ++ map headerVar headers
 where headerVarName :: HeaderName -> String
       headerVarName name = varName (BU.toString (CI.original name))
       headerVar :: Header -> (String, String)
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
