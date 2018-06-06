module API.Pexels.Methods where

import Prelude

import API.Pexels.Search (ApiKey(..), Request, Result, toUrlEncoded)
import Data.Argonaut (Json, decodeJson, getField, jsonEmptyArray)
import Data.Array ((:))
import Data.Either (Either(..), hush)
import Data.FormURLEncoded (encode)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Record.Fold (collect)
import Network.HTTP.Affjax (Affjax, AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(..))
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

send :: forall e a b. Requestable a => Respondable b => AffjaxRequest a -> Affjax e b
send req = affjax req

-- search :: forall e. ApiKey → Validation (Aff (ajax :: AJAX | e)) (Array String) Request Result
-- search apiKey = hoistFnMV $ \req -> runValidation
--   (hoistFn (buildRequest apiKey) >>> affjaxJson >>> stringifyErrs searchResult) req

-- searchResult :: forall m. Monad m => JsValidation m Result
-- searchResult = collect
--   { totalResults: field "total_results" int
--   , nextPage: optionalField "next_page" (Just <$> string)
--   , prevPage: optionalField "prev_page" (Just <$> string)
--   }

search request apiKey = affjax $ apiKey buildRequest 

getResult :: Json -> Either String Result
getResult js = do 
  obj <- decodeJson js 
  pure $ { totalResults: (getField obj "total_results"), 
            nextPage: hush (getField obj "next_page"), 
            prevPage: hush (getField obj "prev_page") }
