module API.Pexels.Types where

import Prelude

import Control.Extend ((<<=))
import Data.Array ((!!))
import Data.Either (Either, hush, note)
import Data.FormURLEncoded (FormURLEncoded, fromArray)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Pexels.Utils (parseUrlencoded)

newtype ApiKey = ApiKey String

type BaseRequest extra = (page :: Int , perPage :: Int | extra)

type SearchRequest = { | BaseRequest (query :: String) }
type CuratedRequest = { | BaseRequest ()}


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


-- TODO: impl

urlToSearchRequest:: String -> (Maybe SearchRequest)
urlToSearchRequest url = do
                          parsedUrl <- hush $ parseUrlencoded url
                          query <- (parsedUrl !! 2) >>= snd
                          page <- (parsedUrl !! 0) >>= snd >>= fromString
                          perPage <- (parsedUrl !! 1) >>= snd >>= fromString
                          pure {query: query, page: page, perPage: perPage }


urlToCuratedRequest:: String -> (Maybe CuratedRequest)
urlToCuratedRequest url = do
                          parsedUrl <- hush $ parseUrlencoded url
                          page <- (parsedUrl !! 0) >>= snd >>= fromString
                          perPage <- (parsedUrl !! 1) >>= snd >>= fromString
                          pure {page: page, perPage: perPage }


type Photo =
  { id:: Int
  ,  width :: Int
  , height :: Int
  , url :: String
  }

type ResultBaseRow extra = (page:: Int, perPage:: Int, photos :: Array Photo | extra)

type CuratedPhotos = { | ResultBaseRow (
    nextPage :: Maybe CuratedRequest, prevPage :: Maybe CuratedRequest) }
type SearchPhotos = { | ResultBaseRow (totalResults :: Int, 
    nextPage :: Maybe SearchRequest, prevPage :: Maybe SearchRequest) }