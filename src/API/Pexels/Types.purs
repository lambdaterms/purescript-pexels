module API.Pexels.Types where

import Prelude
import Data.Array ((!!))
import Data.Either (Either, note)
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

urlToSearchRequest:: String -> (Either String SearchRequest)
urlToSearchRequest url = do
                          parsedUrl <- parseUrlencoded url
                          query <- join $ note"" $ note "" <$> snd <$> (parsedUrl !! 2)
                          page <- (join $ note "" $ note "" <$> snd <$> (parsedUrl !! 0))
                          perPage <- join $ note "" $ note "" <$> snd <$> (parsedUrl !! 1)
                          pageInt <- note "" $ fromString page
                          perPageInt <- note "" $ fromString perPage
                          pure {query: query, page: pageInt, perPage: perPageInt }


urlToCuratedRequest:: String -> (Either String CuratedRequest)
urlToCuratedRequest url = do
                          parsedUrl <- parseUrlencoded url
                          page <- (join $ note "" $ note "" <$> snd <$> (parsedUrl !! 0))
                          perPage <- join $ note "" $ note "" <$> snd <$> (parsedUrl !! 1)
                          pageInt <- note "" $ fromString page
                          perPageInt <- note "" $ fromString perPage
                          pure {page: pageInt, perPage: perPageInt }


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