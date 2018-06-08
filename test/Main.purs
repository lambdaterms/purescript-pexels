module Test.Main where
  
import Prelude

import API.Pexels.Methods (buildRequest, searchWithValidation)
import API.Pexels.Search (ApiKey(..), Request, toUrlEncoded)
import API.Pexels.Validation (getJson, getResultfromJson, stringifyErrs, validateStatus)
import Control.Monad.Aff (Fiber, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Argonaut (Json)
import Data.FormURLEncoded (encode)
import Data.List.Lazy (alterAt)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax)
import Network.HTTP.StatusCode (StatusCode(..))
import Polyform.Validation (V, runValidation)
--structures for tests
simpleRequest1 :: Request
simpleRequest1 = {query: "dog", page: 1, perPage: 15}

simpleRequest1Str :: String
simpleRequest1Str = "query=dog&per_page=15&page=1"

simpleResponse1 :: AffjaxResponse String
simpleResponse1 = {status: StatusCode 200, headers: [], response: ""}

simpleNotJson :: String
simpleNotJson = "aaaabbbccc"

simpleJson :: String
simpleJson = "{\"page\":1,\"per_page\":15}"

simpleJsonWrongFromat :: String
simpleJsonWrongFromat = "{\"total_results\":\"dddd\",\"page\":1,\"per_page\":15}"

simpleJson1 :: String
simpleJson1 = "{\"total_results\":10,\"page\":1,\"per_page\":15}"

--functions for running tests

validateStatusTest = (runValidation validateStatus) simpleResponse1

getJsonTest json = (runValidation getJson) (simpleResponse1 {response = json})

getAndValidateJsonTest json = (runValidation $ stringifyErrs(getResultfromJson) <<< getJson) (simpleResponse1 {response = json})

main :: forall t22.
  Eff
    ( ajax :: AJAX
    , console :: CONSOLE
    | t22
    )
    (Fiber
       ( ajax :: AJAX
       , console :: CONSOLE
       | t22
       )
       Unit
    )
-- TODO: make "real" tests with asserts
main = launchAff $ do
    (a :: AffjaxResponse Json) <- affjax $ buildRequest (ApiKey "secretApiKey") simpleRequest1 
    res1 <- (unsafeStringify <$> validateStatusTest)
    log res1
    res2 <- (unsafeStringify <$> getJsonTest simpleNotJson)
    log res2
    res3 <- (unsafeStringify <$> getJsonTest simpleJson)
    log res3
    res4 <- (unsafeStringify <$> getAndValidateJsonTest simpleJsonWrongFromat)
    log res4
    a <- searchWithValidation (ApiKey "secretApiKey") simpleRequest1
    log $ unsafeStringify a
