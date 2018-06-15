module API.Pexels.Types where

import Prelude

import Data.Either (hush)
import Data.FormURLEncoded (FormURLEncoded, fromArray)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(Tuple))
import Pexels.Utils (extractQuery, findQueryVal, parseUrlencoded)

newtype ApiKey = ApiKey String

type BaseRequest extra = (page :: Int , perPage :: Int | extra)
type SearchRequest = { | BaseRequest (query :: String) }
type CuratedRequest = { | BaseRequest ()}

type Photo =
  { id:: Int
  ,  width :: Int
  , height :: Int
  , url :: String
  }

type ResultBaseRow extra = (page:: Int, perPage:: Int, photos :: Array Photo | extra)

type CuratedPhotos = { | ResultBaseRow
  ( nextPage :: Maybe CuratedRequest
  , prevPage :: Maybe CuratedRequest) }

type SearchPhotos = { | ResultBaseRow 
  ( totalResults :: Int
  , nextPage :: Maybe SearchRequest
  , prevPage :: Maybe SearchRequest) }

searchRequestToUrlEncoded ∷ SearchRequest → FormURLEncoded
searchRequestToUrlEncoded { query, page, perPage } = fromArray $
  [ Tuple "query" (Just query)
  , Tuple "per_page" (Just $ show perPage)
  , Tuple "page" (Just $ show page)
  ]

curatedRequestToUrlEncoded ∷ CuratedRequest → FormURLEncoded
curatedRequestToUrlEncoded { page, perPage } = fromArray $
  [ Tuple "per_page" (Just $ show perPage)
  , Tuple "page" (Just $ show page)
  ]

urlToSearchRequest:: String -> (Maybe SearchRequest)
urlToSearchRequest url = do
  parsedQuery <- (hush <<< parseUrlencoded) =<< (extractQuery url)
  query <- findQueryVal parsedQuery "query"
  page <- findQueryVal parsedQuery "page" >>= fromString
  perPage <- findQueryVal parsedQuery "per_page" >>= fromString
  pure {query: query, page: page, perPage: perPage }

urlToCuratedRequest:: String -> (Maybe CuratedRequest)
urlToCuratedRequest url = do
  parsedQuery <- (hush <<< parseUrlencoded) =<< (extractQuery url)
  page <- findQueryVal parsedQuery "page" >>= fromString
  perPage <- findQueryVal parsedQuery "per_page" >>= fromString
  pure {page: page, perPage: perPage }