module Test.Main where
  
import Prelude

import API.Pexels.Methods (buildRequest, getResult)
import API.Pexels.Search (ApiKey(..), Request, toUrlEncoded)
import Control.Monad.Aff (Fiber, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM.Event.KeyboardEvent (KeyLocation(..))
import Data.Argonaut (Json)
import Data.Either (Either)
import Data.FormURLEncoded (encode)
import Data.Newtype (unwrap)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, affjax)

simpleRequest1 :: Request
simpleRequest1 = {query: "dog", page: 1, perPage: 15}

simpleRequest1Str :: String
simpleRequest1Str = "query=dog&per_page=15&page=1"


encodeURLTest :: Request -> String -> Boolean
encodeURLTest request answer = (encode $ toUrlEncoded request) == answer


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
main = launchAff $ do
    (a :: AffjaxResponse Json) <- affjax $ buildRequest (ApiKey "secretApiKey") simpleRequest1
    liftEff (log $ unsafeStringify $ (getResult a.response))
    