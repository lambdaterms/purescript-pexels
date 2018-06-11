module API.Pexels.Validation where

import Prelude

import API.Pexels.Search (SearchPhotos, Photo, urlToRequest)
import Control.Monad.Aff (Aff)
import Control.Monad.Error.Class (catchError)
import Data.Argonaut (Json, jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Record.Fold (collect)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, affjax)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.StatusCode (StatusCode(..))
import Polyform.Validation (V(Invalid, Valid), Validation, hoistFnMV, hoistFnV, lmapValidation)
import Validators.Json (JsError, JsValidation, array, arrayOf, field, int, optionalField, string)

validateAffjax :: forall t16 t17 t18.
  Requestable t17 => Respondable t16 => Validation
                                          (Aff ( ajax :: AJAX | t18))
                                          (Array String)
                                          (AffjaxRequest t17)
                                          (AffjaxResponse t16)
validateAffjax = hoistFnMV $ \req → do
    (Valid [] <$> affjax req) `catchError` (const $ (pure (Invalid ["AJAX request failed"])))


getJson :: forall t1 t2.
  Monad t1 => Validation t1 (Array String)
                  (AffjaxResponse String)
                  Json

getJson = hoistFnV \response -> case jsonParser response.response of
  Right js -> Valid [] js
  Left err -> Invalid ["Parsing Json problem: " <> err]


validateStatus :: forall t1 t6.
  Monad t1 => Validation t1 (Array String) (AffjaxResponse t6) (AffjaxResponse t6)

validateStatus = hoistFnV (\response -> case response.status of
    StatusCode 200 -> Valid [] response
    _ -> Invalid ["server response status other than 200!"])


getResultfromJson :: forall m. Monad m => JsValidation m SearchPhotos

getResultfromJson = collect
  { totalResults: field "total_results" int
  , nextPage: ((<$>) urlToRequest) <$> (optionalField "next_page" (Just <$> string))
  , prevPage: ((<$>) urlToRequest) <$> (optionalField "prev_page" (Just <$> string))
  , page: field "page" int
  , perPage: field "per_page" int
  , photos: field "photos" $ arrayOf getPhotosfromJson
}

getPhotosfromJson :: forall t.
  Monad t => Validation t (Array JsError) Json Photo
getPhotosfromJson = collect
  { id: field "id" int
  ,  width: field "width" int
  , height: field  "height" int
  , url: field "url" string
  }

stringifyErrs
  :: forall m e a b
   . Show e
  => Monad m
  => Validation m (Array e) a b -> Validation m (Array String) a b
stringifyErrs = lmapValidation (map show)