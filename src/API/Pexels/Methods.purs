module API.Pexels.Methods where

import Prelude

import API.Pexels.Types (ApiKey(ApiKey), CuratedRequest, SearchPhotos, SearchRequest, CuratedPhotos, curatedRequestToUrlEncoded, searchRequestToUrlEncoded)
import API.Pexels.Validation (SearchErrorRow, getCuratedResultfromJson, getJson, getSearchResultfromJson, isOK, validateAffjax, validateStatus)
import Control.Monad.Aff (Aff)
import Data.Array ((:))
import Data.Either (Either(Left))
import Data.FormURLEncoded (encode)
import Data.HTTP.Method (Method(GET))
import Data.Variant (Variant)
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

search :: forall t1 err.
  ApiKey -> SearchRequest -> Aff( ajax :: AJAX | t1) (V (Array 
    (Variant (SearchErrorRow err))) SearchPhotos )
search apiKey request = runValidation 
  ( getSearchResultfromJson <<< getJson <<< (validateStatus isOK) <<< validateAffjax ) (buildSearchRequest apiKey request)

curated :: forall t1 err.
  ApiKey -> CuratedRequest -> Aff( ajax :: AJAX | t1) (V (Array 
    (Variant ( SearchErrorRow err))) CuratedPhotos )
curated apiKey request = runValidation 
  ( getCuratedResultfromJson <<< getJson <<< (validateStatus isOK)<<< validateAffjax ) (buildCuratedRequest apiKey request)