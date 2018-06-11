module API.Pexels.Methods where

import Prelude

import API.Pexels.Search (ApiKey(ApiKey), CuratedRequest, SearchPhotos, SearchRequest, CuratedPhotos, curatedRequestToUrlEncoded, searchRequestToUrlEncoded)
import API.Pexels.Validation (getCuratedResultfromJson, getJson, getSearchResultfromJson, stringifyErrs, validateAffjax, validateStatus)
import Control.Monad.Aff (Aff)
import Data.Array ((:))
import Data.Either (Either(Left))
import Data.FormURLEncoded (encode)
import Data.HTTP.Method (Method(GET))
import Network.HTTP.Affjax (AJAX, AffjaxRequest, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Polyform.Validation (V, runValidation)

buildSearchRequest :: ApiKey -> SearchRequest -> AffjaxRequest Unit
buildSearchRequest (ApiKey apiKey) r =
  let
    query = r # searchRequestToUrlEncoded # encode
    authHeader = RequestHeader "Authorization" apiKey
  in
    defaultRequest
      { url = "https://api.pexels.com/v1/search?" <> query
      , method = Left GET, headers = authHeader : defaultRequest.headers
      }

buildCuratedRequest :: ApiKey -> CuratedRequest -> AffjaxRequest Unit
buildCuratedRequest (ApiKey apiKey) r =
  let
    query = r # curatedRequestToUrlEncoded # encode
    authHeader = RequestHeader "Authorization" apiKey
  in
    defaultRequest
      { url = "https://api.pexels.com/v1/curated?" <> query
      , method = Left GET, headers = authHeader : defaultRequest.headers
      }

searchWithValidation :: forall t1.
  ApiKey -> SearchRequest -> Aff( ajax :: AJAX | t1) (V (Array String) SearchPhotos )
searchWithValidation apiKey request = runValidation 
  (stringifyErrs(getSearchResultfromJson) <<< getJson <<< validateStatus <<< validateAffjax ) (buildSearchRequest apiKey request)

curatedWithValidation :: forall t1.
  ApiKey -> CuratedRequest -> Aff( ajax :: AJAX | t1) (V (Array String) CuratedPhotos )
curatedWithValidation apiKey request = runValidation 
  (stringifyErrs(getCuratedResultfromJson) <<< getJson <<< validateStatus <<< validateAffjax ) (buildCuratedRequest apiKey request)