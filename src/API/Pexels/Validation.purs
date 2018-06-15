module API.Pexels.Validation where

import Prelude

import API.Pexels.Types (CuratedPhotos, Photo, SearchPhotos, urlToCuratedRequest, urlToSearchRequest)
import Data.Argonaut (Json, jsonParser)
import Data.Array (singleton)
import Data.Either (Either(Left, Right))
import Data.Functor.Variant (SProxy(..))
import Data.Record.Fold (collect)
import Data.Variant (Variant, inj)
import Polyform.Validation (V(Invalid, Valid), Validation, hoistFnV)
import Validators.Affjax (HttpErrorRow, AffjaxErrorRow)
import Validators.Json (JsError, arrayOf, field, int, optionalField, string)

type JsonErrorRow (err :: # Type) = (parsingError :: String | err)
type SearchErrorRow (err :: # Type) = HttpErrorRow (JsError (JsonErrorRow (AffjaxErrorRow err)))

getJson 
  :: forall m err
   . Monad m 
  => Validation m
      (Array (Variant (JsonErrorRow err)))
      String
      Json
getJson = hoistFnV \response -> case jsonParser response of
  Right js -> Valid [] js
  Left error -> Invalid  $ singleton $ (inj (SProxy :: SProxy "parsingError") error)

getSearchResultfromJson 
  :: forall err m
   . Monad m 
  => Validation m
      (Array (Variant (JsError err)))
      Json
      SearchPhotos
getSearchResultfromJson = collect
  { totalResults: field "total_results" int
  , nextPage: urlToSearchRequest <$> (optionalField "next_page" string)
  , prevPage: urlToSearchRequest <$> (optionalField "prev_page"  string)
  , page: field "page" int
  , perPage: field "per_page" int
  , photos: field "photos" $ arrayOf getPhotosfromJson
  }

getCuratedResultfromJson 
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      CuratedPhotos
getCuratedResultfromJson = collect
  { nextPage: urlToCuratedRequest <$> (optionalField "next_page" string)
  , prevPage: urlToCuratedRequest <$> (optionalField "prev_page"  string)
  , page: field "page" int
  , perPage: field "per_page" int
  , photos: field "photos" $ arrayOf getPhotosfromJson
  }

getPhotosfromJson 
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      Photo
getPhotosfromJson = collect
  { id: field "id" int
  ,  width: field "width" int
  , height: field  "height" int
  , url: field "url" string
  }