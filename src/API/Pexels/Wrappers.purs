module API.Pexels.Search where

import Prelude

import Data.FormURLEncoded (FormURLEncoded, fromArray)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

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


urlToRequest:: String -> SearchRequest
urlToRequest url = {query: "aaa",page: 5,perPage: 15 }

type Photo =
  { id:: Int
  ,  width :: Int
  , height :: Int
  , url :: String
  }

--     photographer: "Name",
--     src: {
--       original: "https://*.jpg",
--       large: "https://*.jpg",
--       large2x: "https://*.jpg",
--       medium: "https://*.jpg",
--       small: "https://*.jpg",
--       portrait: "https://*.jpg",
--       landscape: "https://*.jpg",
--       tiny: "https://*.jpg"
--     }, (NEXT PHOTOS)]
--   }

--TODO: use this polimorphism in other methods
type ResultBaseRow extra = (page:: Int, perPage:: Int, photos :: Array Photo, 
    nextPage :: Maybe String, prevPage :: Maybe String | extra)

type CuratedPhotos = { | ResultBaseRow () }
type SearchPhotos = { | ResultBaseRow (totalResults :: Int) }