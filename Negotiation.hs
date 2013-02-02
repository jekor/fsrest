module Negotiation (matches, bestMatches) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, char, string, skipSpace, takeTill, inClass, rational)
import Data.Attoparsec.Combinator (sepBy, option)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.List (sortBy, find)
import Data.Maybe (catMaybes)
import qualified System.FilePath.Glob as G

-- Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
-- We have: text/html, text/plain
-- Best: [text/html]

-- Accept: */*
-- We have: text/html, text/plain
-- Best: [text/html, text/plain]

type Quality = Double

bestMatches :: [B.ByteString] -> B.ByteString -> [B.ByteString]
bestMatches available accept =
  case matches available accept of
    [] -> []
    ms -> let highest = snd $ head $ reverse $ sortBy (\a b -> compare (snd a) (snd b)) ms in
            map fst [ x | x <- ms, snd x == highest ]

matches :: [B.ByteString] -> B.ByteString -> [(B.ByteString, Quality)]
matches available accept =
  case parseOnly acceptParser accept of
    Left _ -> [] -- TODO: Log an error?
    -- For now, only negotiate the content type.
    Right acceptable -> catMaybes $ map (flip match ordered) available
                         where ordered = reverse $ sortBy (\a b -> compare (snd a) (snd b)) acceptable
                               match :: B.ByteString -> [(B.ByteString, Quality)] -> Maybe (B.ByteString, Quality)
                               match rep reps = case find (\(r, _) -> G.match (G.compile $ B8.unpack r) (B8.unpack rep)) reps of
                                                  Nothing -> Nothing
                                                  Just (_, q) -> Just (rep, q)

acceptParser :: Parser [(B.ByteString, Quality)]
acceptParser = ((,) <$> (skipSpace *> takeTill (inClass ";, "))
                    <*> (option 1.0 (skipSpace *> char ';' *> skipSpace *> string "q=" *> rational)))
               `sepBy` (skipSpace *> char ',' <* skipSpace)
