module API.Pexels.Validation where

import Prelude

import API.Pexels.Types (CuratedPhotos, Photo, SearchPhotos, urlToCuratedRequest, urlToSearchRequest)
import Data.Argonaut (Json)
import Data.Array (filter, last)
import Data.Maybe (Maybe(..))
import Data.Record.Fold (collect)
import Data.String (Pattern(..), split)
import Data.Variant (Variant)
import Polyform.Validation (Validation, hoistFn, hoistFnV)
import Validators.Json (JsError, arrayOf, failure, field, int, optionalField, string)

getSearchResultfromJson
  ∷ ∀ err m
   . Monad m
  ⇒ Validation m
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
  ∷ ∀ err m
   . Monad m
  ⇒ Validation m
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
  ∷ ∀ err m
   . Monad m
  ⇒ Validation m
      (Array (Variant (JsError err)))
      Json
      Photo
getPhotosfromJson = collect
  { id: field "url" string >>> hoistFn (split (Pattern "/") >>> filter ("" /= _) >>> last) >>> (hoistFnV $ case _ of
      Nothing → failure "Unable to parse out \"id\" value"
      Just i → pure i)
  , width: field "width" int
  , height: field  "height" int
  , src: field "src" $ collect
    { landscape: field "landscape" string
    , large: field "large" string
    , large2x: field "large2x" string
    , medium: field "medium" string
    , original : field "original" string
    , portrait : field "portrait" string
    , small : field "small" string
    , tiny : field "tiny" string
    }
  , url: field "url" string
  }
