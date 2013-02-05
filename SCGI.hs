-- Copyright 2013 Chris Forno

module SCGI (SCGI, runRequest, header, allHeaders, method, path, setHeader, Headers, Body, Status, Response(..)) where

import Control.Applicative ((<$>), (<*>), (<*))
import Control.Arrow (first)
import Control.Monad (liftM, liftM2)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader, asks)
import Control.Monad.State (StateT, runStateT, MonadState, modify)
import Data.Attoparsec.ByteString.Char8 (Parser, IResult(..), parseOnly, parseWith, char, decimal, take, takeTill)
import Data.Attoparsec.Combinator (many1)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.Char (toUpper)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import System.IO (Handle)

import Prelude hiding (take)

type Headers = Map (CI B.ByteString) B.ByteString
type Body = BL.ByteString
type Status = BL.ByteString
data Response = Response Status Body

newtype SCGI a = SCGI (ReaderT Headers (StateT Headers IO) a)
    deriving (Functor, Monad, MonadIO, MonadState Headers, MonadReader Headers)

runSCGI :: Headers -> SCGI Response -> IO (Response, Headers)
runSCGI headers (SCGI r) = runStateT (runReaderT r headers) M.empty

-- |Lookup a request header.
header :: B.ByteString -- ^ the header name (key)
       -> SCGI (Maybe B.ByteString) -- ^ the header value if found
header name = asks (M.lookup (CI.mk name))

-- |Return all request headers as a list in the format they were received from the web server.
allHeaders :: SCGI [(B.ByteString, B.ByteString)] -- ^ an association list of header: value pairs
allHeaders = asks (map (first CI.original) . M.toList)

-- |Get the request method (GET, POST, etc.). You could look the header up
-- yourself, but this normalizes the method name to uppercase.
method :: SCGI (Maybe B.ByteString) -- ^ the method if found
method = liftM (B8.map toUpper) `liftM` header "REQUEST_METHOD"

-- |Get the requested path. According to the spec, this can be complex, and
-- actual CGI implementations diverge from the spec. I've found this to work,
-- even though it doesn't seem correct or intuitive.
path :: SCGI (Maybe B.ByteString) -- ^ the path if found
path = do
  path1 <- header "SCRIPT_NAME"
  path2 <- header "PATH_INFO"
  return $ liftM2 B.append path1 path2

-- |Set a response header.
setHeader :: B.ByteString -- ^ the header name (key)
          -> B.ByteString -- ^ the header value
          -> SCGI ()
setHeader name value = modify (M.insert (CI.mk name) value)

-- |Run a request in the SCGI monad.
runRequest :: Handle -- ^ the handle connected to the web server (from 'accept')
           -> (Body -> SCGI Response) -- ^ the action to run in the SCGI monad
           -> IO () -- ^ nothing is returned, the result of the action is written back to the server
runRequest h f = do
  -- Note: This could potentially read any amount of data into memory.
  -- For now, I'm leaving it up to the SCGI implementation in the server to block large header payloads.
  --
  -- First, parse the netstring containing the headers. If we tried to avoid this step the syntax for
  -- the headers would be ambiguous.
  result <- parseWith (B.hGetSome h 4096) netstringParser ""
  case result of
    Done rest headerString ->
      case parseOnly (many1 headerParser) headerString of
        Left e -> error e
        Right headers -> do
          -- CONTENT_LENGTH is required by the SCGI spec. Without it, we
          -- wouldn't know when we'd reached the end of the request.
          --
          -- The header Map uses case-insensitive keys.
          let headerMap = M.fromList $ map (first CI.mk) headers
              len' = B8.readInt $ M.findWithDefault (error "CONTENT_LENGTH missing from request") "CONTENT_LENGTH" headerMap
          case len' of
            Just (len, _) -> do
              -- We have probably read past the end of the header. Take the
              -- rest of the unparsed string and what remains to be read
              -- (determined from the CONTENT_LENGTH) and make that the body.
              let c = fromIntegral (len - B.length rest)
              body <- (BL.fromChunks [rest] `BL.append`) `liftM` (if c > 0 then BL.hGet h c else return "")
              (Response status body', headers') <- runSCGI headerMap (f body)
              -- Every SCGI response must include a status line first.
              BL.hPutStr h $ BL.concat ["Status: ", status, "\r\n"]
              -- Output the headers returned by the SCGI action.
              mapM_ (\(k, v) -> B.hPutStr h $ B.concat [CI.original k, ": ", v, "\r\n"]) $ M.toList headers'
              BL.hPutStr h "\r\n"
              -- Finally, output the body.
              BL.hPutStr h body'
            _ -> error "Failed to parse CONTENT_LENGTH."
    _ -> error "Failed to parse SCGI request."

-- http://cr.yp.to/proto/netstrings.txt
netstringParser :: Parser B.ByteString
netstringParser = do
  count <- decimal <* char ':'
  take count <* char ','

headerParser :: Parser (B.ByteString, B.ByteString)
headerParser = (,) <$> cStringParser <*> cStringParser

cStringParser :: Parser B.ByteString
cStringParser = takeTill (== '\NUL') <* char '\NUL'
