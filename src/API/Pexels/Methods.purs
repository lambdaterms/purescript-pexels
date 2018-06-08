module API.Pexels.Methods where

import Prelude

import API.Pexels.Search (ApiKey(ApiKey), Request, SearchPhotos, toUrlEncoded)
import API.Pexels.Validation (getJson, getResultfromJson, stringifyErrs, validateAffjax, validateStatus)
import Control.Monad.Aff (Aff)
import Data.Array ((:))
import Data.Either (Either(Left))
import Data.FormURLEncoded (encode)
import Data.HTTP.Method (Method(GET))
import Network.HTTP.Affjax (AJAX, AffjaxRequest, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Polyform.Validation (V, runValidation)

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

searchWithValidation :: forall t1.
  ApiKey -> Request -> Aff( ajax :: AJAX | t1) (V (Array String) SearchPhotos )
searchWithValidation apiKey request = runValidation 
  (stringifyErrs(getResultfromJson) <<< getJson <<< validateStatus <<< validateAffjax ) (buildRequest apiKey request)