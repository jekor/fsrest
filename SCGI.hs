module SCGI (SCGI, runRequest, header, setHeader, allHeaders, method, Body, Response(..)) where

import Control.Applicative ((<$>), (<*>), (<*))
import Control.Arrow (first)
import Control.Monad (liftM)
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

runSCGI :: Headers -> (SCGI Response) -> IO (Response, Headers)
runSCGI headers (SCGI r) = runStateT (runReaderT r headers) M.empty

header :: B.ByteString -> SCGI (Maybe B.ByteString)
header name = asks (M.lookup (CI.mk name))

allHeaders :: SCGI [(B.ByteString, B.ByteString)]
allHeaders = asks (map (first CI.original) . M.toList)

setHeader :: B.ByteString -> B.ByteString -> SCGI ()
setHeader name value = modify (M.insert (CI.mk name) value)

method :: SCGI (Maybe B.ByteString)
method = asks (liftM (B8.map toUpper) . (M.lookup (CI.mk "REQUEST_METHOD")))

runRequest :: Handle -> (Body -> SCGI Response) -> IO ()
runRequest h f = do
  -- Note: This could potentially read any amount of data into memory.
  -- For now, I'm leaving it up to the SCGI implementation in the server to block large header payloads.
  result <- parseWith (B.hGetSome h 4096) netstringP ""
  case result of
    Done rest headerString ->
      case parseOnly (many1 headerP) headerString of
        Left e -> error e
        Right headers -> do
          let headerMap = M.fromList $ map (first CI.mk) headers
              len = B8.readInt $ M.findWithDefault (error "CONTENT_LENGTH missing from request") "CONTENT_LENGTH" headerMap
          case len of
            Just (len', _) -> do
              let c = fromIntegral (len' - B.length rest)
              body <- (BL.fromChunks [rest] `BL.append`) `liftM` (if c > 0 then BL.hGet h c else return "")
              (Response status body', headers') <- runSCGI headerMap (f body)
              BL.hPutStr h $ BL.concat ["Status: ", status, "\r\n"]
              mapM_ (\(k, v) -> B.hPutStr h $ B.concat [CI.original k, ": ", v, "\r\n"]) $ M.toList headers'
              BL.hPutStr h "\r\n"
              BL.hPutStr h body'
            _ -> error "Failed to parse CONTENT_LENGTH."
    _ -> error "Failed to parse SCGI request."

netstringP :: Parser B.ByteString
netstringP = do
  count <- decimal <* char ':'
  take count <* char ','

headerP :: Parser (B.ByteString, B.ByteString)
headerP = (,) <$> cStringP <*> cStringP

cStringP :: Parser B.ByteString
cStringP = takeTill (== '\NUL') <* char '\NUL'
