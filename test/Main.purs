module Test.Main where
  
import Prelude

import API.Pexels.Search (Request, toUrlEncoded)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.FormURLEncoded (encode)

simpleRequest1 :: Request
simpleRequest1 = {query: "dog", page: 1, perPage: 15}

simpleRequest1Str :: String
simpleRequest1Str = "query=dog&per_page=15&page=1"


encodeURLTest :: Request -> String -> Boolean
encodeURLTest request answer = (encode $ toUrlEncoded request) == answer

main :: forall t1.
  Eff
    ( console :: CONSOLE
    | t1
    )
    Unit
main = do
    log $ show $ encodeURLTest simpleRequest1 simpleRequest1Str
    
    
