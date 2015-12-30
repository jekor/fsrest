module Media ( MediaType(..), parseMediaType, printMediaType
             , mediaTypeToFileName, mediaTypeFromFileName
             , mediaTypeMatches, best
             ) where

import Data.Attoparsec.ByteString.Char8 hiding (match)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import Data.Function (on)
import Data.List (find, maximumBy)
import Data.Maybe (mapMaybe)

data MediaType = MediaType { mtType :: B.ByteString
                           , mtSubType :: B.ByteString
                           } deriving Show
-- We ignore parameters for now
-- TODO: Charset and Language

instance Eq MediaType where
  (MediaType aType aSubType) == (MediaType bType bSubType) =
    aType `globEqual` bType && aSubType `globEqual` bSubType
   where globEqual "*" _ = True
         globEqual _ "*" = True
         globEqual a b = a == b

type Quality = Double
type Accept = [(MediaType, Quality)]

mediaTypeToFileName :: MediaType -> FilePath
mediaTypeToFileName mt = BU.toString (B.concat [mtType mt, ".", mtSubType mt])

mediaTypeFromFileName :: FilePath -> Maybe MediaType
mediaTypeFromFileName = parseMediaType . BU.fromString . dotToSlash
       -- replace just the first . with a /
 where dotToSlash [] = []
       dotToSlash ('.':xs) = '/' : xs
       dotToSlash (x:xs) = x : dotToSlash xs

parseMediaType :: B.ByteString -> Maybe MediaType
parseMediaType s =
  case parseOnly mediaTypeP s of
    Left _ -> Nothing
    Right mt -> Just mt

printMediaType :: MediaType -> B.ByteString
printMediaType mt = B.concat [mtType mt, "/", mtSubType mt]

-- Our parser is a bit loose.
mediaTypeP :: Parser MediaType
mediaTypeP = MediaType <$> (takeWhile1 isAlpha_ascii <* char '/')
                       <*> takeWhile1 (\ c -> isAlpha_ascii c || isDigit c || inClass "-.+" c)

-- e.g. text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
acceptP :: Parser Accept
acceptP = ((,) <$> globMediaTypeP
               <*> option 1.0 (skipSpace *> char ';' *> skipSpace *> string "q=" *> rational))
          `sepBy` (skipSpace *> char ',' <* skipSpace)
 where globMediaTypeP = MediaType <$> (choice [string "*", takeWhile1 isAlpha_ascii] <* char '/')
                                  <*> choice [string "*", takeWhile1 (\ c -> isAlpha_ascii c || isDigit c || inClass "-.+" c)]

-- Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
-- acceptP = [("text/html", 1.0), ("application/xhtml+xml", 1.0), ("application/xml", 0.9), ("*/*", 0.8)]
-- Available: text/html, text/plain
-- matches = [("text/html", 1.0), ("text/plain", 0.8)]
-- best . matches = [(text/html, 1.0)]

-- Accept: */*
-- acceptP = [("*/*", 1.0)]
-- Available: text/html, text/plain
-- matches = [("text/html", 1.0), ("text/plain", 1.0)]
-- best . matches = [("text/html", 1.0), ("text/plain", 1.0)]

-- Find all representations that match the client's Accept header.
-- TODO: Is it possible for a representation to be returned more than once with different qualities?
mediaTypeMatches :: [MediaType] -> B.ByteString -> [(MediaType, Quality)]
mediaTypeMatches available acceptHeader =
  case parseOnly acceptP acceptHeader of
    Left _ -> [] -- TODO: Log an error?
    -- For now, only negotiate the content type.
    Right accept -> mapMaybe (`match` accept) available
 where match :: MediaType -> [(MediaType, Quality)] -> Maybe (MediaType, Quality)
       match mt mts = case find ((== mt) . fst) mts of
                        Nothing -> Nothing
                        Just (_, q) -> Just (mt, q)

-- Find the best matches (all of the highest quality) from a list of resource representation matches.
best :: [(MediaType, Quality)] -> [MediaType]
best ms = map fst [ x | x <- ms, snd x == highest ]
 where highest = snd $ maximumBy (compare `on` snd) ms
