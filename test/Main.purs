module Test.Main where

import Prelude

import API.Pexels.Methods (curated, search)
import API.Pexels.Types (ApiKey(ApiKey), CuratedRequest, SearchRequest)
import Control.Monad.Aff (Fiber, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Global.Unsafe (unsafeStringify)
import Key (key)
import Network.HTTP.Affjax (AJAX)

--structures for tests
simpleRequest1 :: SearchRequest
simpleRequest1 = {query: "dog", page: 1, perPage: 15}

simpleCuratedRequest :: CuratedRequest
simpleCuratedRequest = {page: 1, perPage: 15}


main
  :: forall t22
   . Eff ( ajax :: AJAX, console :: CONSOLE| t22)
      (Fiber( ajax :: AJAX, console :: CONSOLE| t22) Unit )
-- TODO: make "real" tests with asserts
main = launchAff $ do 
  a <- search (ApiKey key) simpleRequest1
  log $ unsafeStringify a
  res6 <- curated (ApiKey key) simpleCuratedRequest
  log $ unsafeStringify res6
