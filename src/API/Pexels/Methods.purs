module API.Pexels.Methods where

import Prelude

import API.Pexels.Search (ApiKey(..), Request, Result, toUrlEncoded)
import API.Pexels.Validation (affjaxJson, stringifyErrs)
import Control.Alternative ((<|>))
import Control.Monad.Aff (Aff)
import Data.Array ((:))
import Data.Either (Either(..))
import Data.FormURLEncoded (encode)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Record.Fold (collect)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Polyform.Validation (Validation(..), hoistFn, hoistFnMV, runValidation)
import Validators.Json (JsValidation, field, int, optionalField, string)

buildRequest :: ApiKey -> Request -> AffjaxRequest Unit
buildRequest (ApiKey apiKey) r =
  let
    query = r # toUrlEncoded # encode
    authHeader = RequestHeader "Authorization" apiKey
  in
    defaultRequest
      { url = "https://api.pexels.com/v1/search?" <> query
      , method = Left GET, headers = authHeader : defaultRequest.headers
      }

search :: forall e. ApiKey → Validation (Aff (ajax :: AJAX | e)) (Array String) Request Result
search apiKey = hoistFnMV $ \req -> runValidation
  (hoistFn (buildRequest apiKey) >>> affjaxJson >>> stringifyErrs searchResult) req

searchResult :: forall m. Monad m => JsValidation m Result
searchResult = collect
  { totalResults: field "total_results" int
  , nextPage: optionalField "next_page" (Just <$> string)
  , prevPage: optionalField "prev_page" (Just <$> string)
  }
