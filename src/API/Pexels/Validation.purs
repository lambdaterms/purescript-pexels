module API.Pexels.Validation where

import Prelude

import API.Pexels.Search (CuratedPhotos, SearchPhotos, Photo)
import Control.Monad.Aff (Aff)
import Control.Monad.Error.Class (catchError)
import Data.Argonaut (Json, jsonParser)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Functor.Variant (SProxy(..))
import Data.Maybe (Maybe(..))
import Data.Record.Fold (collect)
import Data.Variant (Variant, inj)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, affjax)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.StatusCode (StatusCode(..))
import Polyform.Validation (V(Invalid, Valid), Validation, hoistFnMV, hoistFnV)
import Validators.Json (JsError, arrayOf, field, int, optionalField, string)


type HttpErrorRow (err :: # Type) = (wrongHttpStatus :: StatusCode | err)
type JsonErrorRow (err :: # Type) = (parsingError :: String | err)
type AffjaxErrorRow (err :: # Type) = (remoteError :: String | err)





validateAffjax :: forall req res ext err.
  Requestable req => Respondable res => Validation
                                          (Aff ( ajax :: AJAX | ext))
                                          (Array (Variant (AffjaxErrorRow err)))
                                          (AffjaxRequest req)
                                          (AffjaxResponse res)
validateAffjax = hoistFnMV $ \req → do
    (Valid [] <$> affjax req) `catchError` (\e -> pure (Invalid $ singleton $ (inj (SProxy :: SProxy "remoteError") $ show e)))


validateStatus :: forall m err res.
  Monad m => Validation m
                (Array (Variant (HttpErrorRow err)))
                (AffjaxResponse res)
                (AffjaxResponse res)
validateStatus = hoistFnV (\response -> case response.status of
    StatusCode 200 -> Valid [] response
    _ -> Invalid $ singleton $ (inj (SProxy :: SProxy "wrongHttpStatus") response.status))



getJson :: forall m err.
  Monad m => Validation m
                 (Array (Variant (JsonErrorRow err)))
                 (AffjaxResponse String)
                 Json
getJson = hoistFnV \response -> case jsonParser response.response of
  Right js -> Valid [] js
  Left error -> Invalid  $ singleton $ (inj (SProxy :: SProxy "parsingError") error)


getSearchResultfromJson :: forall err m.
  Monad m => Validation m
                  (Array (Variant (JsError err) ))
                  Json
                  SearchPhotos
getSearchResultfromJson = collect
  { totalResults: field "total_results" int
  , nextPage: optionalField "next_page" (Just <$> string)
  , prevPage: optionalField "prev_page" (Just <$> string)
  , page: field "page" int
  , perPage: field "per_page" int
  , photos: field "photos" $ arrayOf getPhotosfromJson
}

getCuratedResultfromJson :: forall err m.
  Monad m => Validation m
                  (Array (Variant (JsError err) ))
                  Json
                  CuratedPhotos

getCuratedResultfromJson = collect
  { nextPage: optionalField "next_page" (Just <$> string)
  , prevPage: optionalField "prev_page" (Just <$> string)
  , page: field "page" int
  , perPage: field "per_page" int
  , photos: field "photos" $ arrayOf getPhotosfromJson
}

getPhotosfromJson :: forall err m.
  Monad m => Validation m
                  (Array (Variant (JsError err) ))
                  Json
                  Photo
getPhotosfromJson = collect
  { id: field "id" int
  ,  width: field "width" int
  , height: field  "height" int
  , url: field "url" string
  }
