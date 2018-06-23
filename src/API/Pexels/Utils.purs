-- | Parser for the `application/x-www-form-urlencoded` format, commonly used
-- | for query strings and POST request bodies.
module Pexels.Utils
  ( parseUrlencoded
  , extractQuery
  , findQueryVal
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (filter, head, (!!))
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (split, joinWith, Pattern(Pattern))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Global (decodeURIComponent)

toTuple :: Array String -> Either String (Tuple String (Maybe String))
toTuple kv =
  case kv of
    [key] ->
      pure (Tuple (decodeURIComponent key) Nothing)
    [key, value] ->
      pure (Tuple (decodeURIComponent key) (Just (decodeURIComponent value)))
    parts ->
      throwError ("Invalid form key-value pair: " <> joinWith " " parts)

parseUrlencoded :: String → Either String (Array (Tuple String (Maybe String)))
parseUrlencoded = split (Pattern "&")
  >>> Array.filter (_ /= "")
  >>> map (split (Pattern "="))
  >>> map toTuple >>> sequence

extractQuery :: String -> Maybe String
extractQuery url = (split (Pattern "?") url) !! 1

findQueryVal :: Array (Tuple String (Maybe String)) -> String -> Maybe String
findQueryVal parsedUrl label = do
  pair <- head (filter (\(p) -> fst p == label) parsedUrl)
  snd pair