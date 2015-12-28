module Media (MediaType(..), parseMediaType, mediaTypeFileName) where

import Text.Parsec
import Text.Parsec.String

data MediaType = MediaType { mtType :: String
                           , mtSubType :: String
                           } -- We ignore parameters for now

parseMediaType :: String -> Maybe MediaType
parseMediaType s =
  case parse mediaType "" s of
    Left _ -> Nothing
    Right mt -> Just mt

-- Our parser is a bit loose.
mediaType :: Parser MediaType
mediaType = do
  type' <- many1 letter
  char '/'
  subType <- many1 (choice [letter, digit, oneOf "-.+"])
  return (MediaType type' subType)

mediaTypeFileName :: MediaType -> FilePath
mediaTypeFileName mt = mtType mt ++ "." ++ mtSubType mt
