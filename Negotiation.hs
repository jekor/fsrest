-- Copyright 2013 Chris Forno

module Negotiation (matches, best, Quality) where

import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, char, string, skipSpace, takeTill, inClass, rational)
import Data.Attoparsec.Combinator (sepBy, option)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Function (on)
import Data.List (sortBy, find, maximumBy)
import Data.Maybe (mapMaybe)
import qualified System.FilePath.Glob as G

-- Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
-- Available: text/html, text/plain
-- acceptParser = [("text/html", 1.0), ("application/xhtml+xml", 1.0), ("application/xml", 0.9), ("*/*", 0.8)]
-- matches = [("text/html", 1.0), ("text/plain", 0.8)]
-- best . matches = [(text/html, 1.0)]

-- Accept: */*
-- Available: text/html, text/plain
-- acceptParser = [("*/*", 1.0)]
-- matches = [("text/html", 1.0), ("text/plain", 0.8)]
-- best . matches = [("text/html", 1.0), ("text/plain", 0.8)]

type Quality = Double

-- TODO: Is it possible for a representation to be returned more than once with different qualities?
-- | Find all representations that match the client's Accept header.
matches :: [B.ByteString] -- ^ the available representations for this resource
        -> B.ByteString -- ^ the Accept header value from the client
        -> [(B.ByteString, Quality)] -- ^ a associative list of matches as (representation, quality) pairs (where representation is a member of the list of available representations).
matches available accept =
  case parseOnly acceptParser accept of
    Left _ -> [] -- TODO: Log an error?
    -- For now, only negotiate the content type.
    Right acceptable -> mapMaybe (`match` ordered) available
                         where ordered = sortBy (flip compare `on` snd) acceptable
                               match :: B.ByteString -> [(B.ByteString, Quality)] -> Maybe (B.ByteString, Quality)
                               match rep reps = case find (\(r, _) -> G.match (G.compile $ B8.unpack r) (B8.unpack rep)) reps of
                                                  Nothing -> Nothing
                                                  Just (_, q) -> Just (rep, q)

-- | Find the best matches (all of the highest quality) from a list of resource representation matches.
best :: [(B.ByteString, Quality)] -- ^ a list of valid matches as (representation, quality) pairs
     -> [B.ByteString] -- ^ the best matches from the list
best [] = []
best ms = let highest = snd $ maximumBy (compare `on` snd) ms in
            map fst [ x | x <- ms, snd x == highest ]

-- e.g. text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
acceptParser :: Parser [(B.ByteString, Quality)]
acceptParser = ((,) <$> (skipSpace *> takeTill (inClass ";, "))
                    <*> option 1.0 (skipSpace *> char ';' *> skipSpace *> string "q=" *> rational))
               `sepBy` (skipSpace *> char ',' <* skipSpace)
