module API.Pexels.Validation where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff)
import Control.Monad.Error.Class (catchError)
import Data.Argonaut (Json, jsonParser)
import Data.Either (Either(..))
import Data.Foldable (find)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Record.Fold (collect)
import Data.String (Pattern(..), contains)
import Data.These (These(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, AffjaxRequest, affjax, defaultRequest)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.StatusCode (StatusCode(..))
import Polyform.Validation (V(Invalid, Valid), Validation, hoistFn, hoistFnMV, hoistFnV, lmapValidation)
import Validators.Json (JsValidation, arrayOf, elem, fail, field, int, optionalField, string)

affjaxRequest
  :: forall req eff
   . Requestable req
  => Validation
      (Aff (ajax :: AJAX | eff))
      (Array String)
      (AffjaxRequest req)
      String
affjaxRequest = validateStatus <<< validateAffjax
  where
  validateAffjax = hoistFnMV $ \req → do
    (Valid [] <$> affjax req) `catchError` (const $ (pure (Invalid ["AJAX request failed"])))
  validateStatus = hoistFnV $ \response -> case response.status of
    StatusCode 200 -> Valid [] response.response
    StatusCode s ->
      Invalid ["API bad response:\n" <> show s <> "\n" <> "\"" <> response.response <> "\""]

affjaxJson
  :: forall eff req
   . Requestable req
  => Validation
      (Aff ( ajax :: AJAX | eff))
      (Array String)
      (AffjaxRequest req)
      Json
affjaxJson = parseJson <<< affjaxRequest
  where
  parseJson = hoistFnV $ \response -> case jsonParser response of
    Right json -> Valid [] json
    Left e -> Invalid ["Json parsing error: " <> e]

getJson
  :: forall eff
   . Validation
      (Aff ( ajax :: AJAX | eff))
      (Array String)
      String
      Json
getJson =
  affjaxJson <<< hoistFn (defaultRequest{ url = _, method = Left GET })


stringifyErrs
  :: forall m e a b
   . Show e
  => Monad m
  => Validation m (Array e) a b -> Validation m (Array String) a b
stringifyErrs = lmapValidation (map show)



