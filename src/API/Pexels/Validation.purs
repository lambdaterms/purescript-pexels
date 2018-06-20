module API.Pexels.Validation where

import Prelude

import API.Pexels.Types (CuratedPhotos, Photo, SearchPhotos, urlToCuratedRequest, urlToSearchRequest)
import Data.Argonaut (Json)
import Data.Record.Fold (collect)
import Data.Variant (Variant)
import Polyform.Validation (Validation)
import Validators.Json (JsError, arrayOf, field, int, optionalField, string)

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