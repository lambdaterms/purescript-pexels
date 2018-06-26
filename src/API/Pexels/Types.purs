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


-- original - The size of the original image is given with the attributes width and height.
-- large - This image has a maximum width of 940px and a maximum height of 650px. It has the aspect ratio of the original image.
-- large2x - This image has a maximum width of 1880px and a maximum height of 1300px. It has the aspect ratio of the original image.
-- medium - This image has a height of 350px and a flexible width. It has the aspect ratio of the original image.
-- small - This image has a height of 130px and a flexible width. It has the aspect ratio of the original image.
-- portrait - This image has a width of 800px and a height of 1200px.
-- landscape - This image has a width of 1200px and height of 627px.
-- tiny - This image has a width of 280px and height of 200px.

type Photo =
  { id :: String
  , width :: Int
  , height :: Int
  , src ::
    { landscape :: String
    , large :: String
    , large2x :: String
    , medium :: String
    , original :: String
    , portrait :: String
    , small :: String
    , tiny :: String
    }
  , url :: String
  }


type ResultBaseRow extra = (page :: Int, perPage :: Int, photos :: Array Photo | extra)

type CuratedPhotosRow = ResultBaseRow
  ( nextPage :: Maybe CuratedRequest
  , prevPage :: Maybe CuratedRequest
  )
type CuratedPhotos = Record CuratedPhotosRow

type SearchPhotosRow = ResultBaseRow
  ( totalResults :: Int
  , nextPage :: Maybe SearchRequest
  , prevPage :: Maybe SearchRequest
  )
type SearchPhotos = Record SearchPhotosRow

searchRequestToUrlEncoded :: SearchRequest -> FormURLEncoded
searchRequestToUrlEncoded { query, page, perPage } = fromArray $
  [ Tuple "query" (Just query)
  , Tuple "per_page" (Just $ show perPage)
  , Tuple "page" (Just $ show page)
  ]

curatedRequestToUrlEncoded :: CuratedRequest -> FormURLEncoded
curatedRequestToUrlEncoded { page, perPage } = fromArray $
  [ Tuple "per_page" (Just $ show perPage)
  , Tuple "page" (Just $ show page)
  ]

urlToSearchRequest :: String -> (Maybe SearchRequest)
urlToSearchRequest url = do
  parsedQuery <- (hush <<< parseUrlencoded) =<< (extractQuery url)
  query <- findQueryVal parsedQuery "query"
  page <- findQueryVal parsedQuery "page" >>= fromString
  perPage <- findQueryVal parsedQuery "per_page" >>= fromString
  pure {query: query, page: page, perPage: perPage }

urlToCuratedRequest :: String -> (Maybe CuratedRequest)
urlToCuratedRequest url = do
  parsedQuery <- (hush <<< parseUrlencoded) =<< (extractQuery url)
  page <- findQueryVal parsedQuery "page" >>= fromString
  perPage <- findQueryVal parsedQuery "per_page" >>= fromString
  pure {page: page, perPage: perPage }
