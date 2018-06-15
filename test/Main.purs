module Test.Main where
  
import Prelude

import API.Pexels.Methods (buildSearchRequest, curated, search)
import API.Pexels.Types (ApiKey(ApiKey), CuratedRequest, SearchRequest)
import API.Pexels.Validation (getJson, getSearchResultfromJson)
import Control.Monad.Aff (Fiber, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Global.Unsafe (unsafeStringify)
import Key (key)
import Network.HTTP.Affjax (AJAX, AffjaxResponse)
import Network.HTTP.StatusCode (StatusCode(..))
import Polyform.Validation (runValidation)
import Validators.Affjax (affjax, isStatusOK, status)

--structures for tests
simpleRequest1 :: SearchRequest
simpleRequest1 = {query: "dog", page: 1, perPage: 15}

rJson :: String
rJson = "{\"total_results\":876,\"page\":1,\"per_page\":15,\"photos\":[],\"next_page\":\"https://api.pexels.com/v1/search/?page=2&per_page=15&query=dog\"}"

simpleCuratedRequest :: CuratedRequest
simpleCuratedRequest = {page: 1, perPage: 15}

simpleRequest1Str :: String
simpleRequest1Str = "query=dog&per_page=15&page=1"

simpleResponse1 :: AffjaxResponse String
simpleResponse1 = {status: StatusCode 200, headers: [], response: ""}

simpleNotJson :: String
simpleNotJson = "aaaabbbccc"

simpleJson :: String
simpleJson = "{\"page\":1,\"per_page\":15, \"photos\": {}}"

simpleJsonWrongFromat :: String
simpleJsonWrongFromat = "{\"total_results\":\"dddd\",\"page\":1,\"per_page\":15}"

simpleJson1 :: String
simpleJson1 = "{\"total_results\":10,\"page\":1,\"per_page\":15}"

--functions for running tests

validateStatusTest = (runValidation $ status isStatusOK) simpleResponse1

getJsonTest json = (runValidation getJson) json

getAndValidateJsonTest json = (runValidation $ getSearchResultfromJson <<< getJson) json

main 
  :: forall t22
   . Eff ( ajax :: AJAX, console :: CONSOLE| t22)
      (Fiber( ajax :: AJAX, console :: CONSOLE| t22) Unit )
-- TODO: make "real" tests with asserts
main = launchAff $ do 
  res1 <- (unsafeStringify <$> validateStatusTest)
  log res1
  res2 <- (unsafeStringify <$> getJsonTest simpleNotJson)
  log res2
  res3 <- (unsafeStringify <$> getJsonTest rJson)
  log res3
  res4 <- (unsafeStringify <$> getAndValidateJsonTest rJson)
  log res4
  resa <- (unsafeStringify <$>  runValidation  (getJson <<< (status isStatusOK) <<< affjax ) (buildSearchRequest (ApiKey key) simpleRequest1))
  log resa
  a <- search (ApiKey key) simpleRequest1
  log $ unsafeStringify a
  res6 <- curated (ApiKey key) simpleCuratedRequest
  log $ unsafeStringify res6
