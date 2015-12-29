{-# LANGUAGE FlexibleInstances #-}

module Headers (headerEnvVars, readHeaders) where

import Data.Aeson (ToJSON(..), object, (.=), encode)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BU
import qualified Data.CaseInsensitive as CI
import Data.Char (isSpace, isAlphaNum, toUpper)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (Header, HeaderName)
import System.IO (Handle, hIsEOF)

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
       headerVarName name = map varChar (BU.toString (CI.original name))
       varChar c | isAlphaNum c = toUpper c
                 | otherwise = '_'
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
