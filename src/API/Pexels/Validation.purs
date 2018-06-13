module API.Pexels.Validation where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Error.Class (catchError)
import Data.Argonaut (Json, jsonParser)
import Data.Array (singleton)
import Data.Either (Either(..))
import Data.Functor.Variant (SProxy(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Record.Fold (collect)
import Data.Variant (Variant, inj)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, affjax)
import Network.HTTP.Affjax.Request (class Requestable)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.StatusCode (StatusCode(..))
import Polyform.Validation (V(Invalid, Valid), Validation, hoistFnMV, hoistFnV)
import Validators.Json (arrayOf, field, int, optionalField, string)


type HttpResponseErrorRow (err :: # Type) = (wrongHttpStatus :: StatusCode | err)
type JsonErrorRow (err :: # Type) = (parsingError :: String | err)
type AffjaxErrorRow (err :: # Type) = (remoteError :: String | err)





validateAffjax :: forall t16 t17 t18 err.
  Requestable t17 => Respondable t16 => Validation
                                          (Aff ( ajax :: AJAX | t18))
                                          (Array (Variant (AffjaxErrorRow err)))
                                          (AffjaxRequest t17)
                                          (AffjaxResponse t16)
validateAffjax = hoistFnMV $ \req → do
    (Valid [] <$> affjax req) `catchError` (\e -> pure (Invalid $ singleton $ (inj (SProxy :: SProxy "remoteError") $ show e)))


validateStatus :: forall t1 t18 t6.
  Monad t1 => Validation t1
                (Array
                   (Variant
                      ( wrongHttpStatus :: StatusCode
                      | t18
                      )
                   )
                )
                { status :: StatusCode
                | t6
                }
                { status :: StatusCode
                | t6
                }
                
validateStatus = hoistFnV (\response -> case response.status of
    StatusCode 200 -> Valid [] response
    _ -> Invalid $ singleton $ (inj (SProxy :: SProxy "wrongHttpStatus") response.status))



getJson :: forall t59 t63 t79 err.
  Monad t59 => Validation t59
                 (Array
                    (Variant
                       ( JsonErrorRow err)
                    )
                 )
                 { response :: String
                 | t63
                 }
                 Json
getJson = hoistFnV \response -> case jsonParser response.response of
  Right js -> Valid [] js
  Left error -> Invalid  $ singleton $ (inj (SProxy :: SProxy "parsingError") error)


getSearchResultfromJson :: forall t143 t144.
  Monad t144 => Validation t144
                  (Array
                     (Variant
                        ( jsError :: { path :: List String
                                     , msg :: String
                                     }
                        | t143
                        )
                     )
                  )
                  Json
                  { totalResults :: Int
                  , prevPage :: Maybe String
                  , photos :: Array
                                { width :: Int
                                , url :: String
                                , id :: Int
                                , height :: Int
                                }
                  , perPage :: Int
                  , page :: Int
                  , nextPage :: Maybe String
                  }
getSearchResultfromJson = collect
  { totalResults: field "total_results" int
  , nextPage: optionalField "next_page" (Just <$> string)
  , prevPage: optionalField "prev_page" (Just <$> string)
  , page: field "page" int
  , perPage: field "per_page" int
  , photos: field "photos" $ arrayOf getPhotosfromJson
}

getCuratedResultfromJson :: forall t286 t287.
  Monad t287 => Validation t287
                  (Array
                     (Variant
                        ( jsError :: { path :: List String
                                     , msg :: String
                                     }
                        | t286
                        )
                     )
                  )
                  Json
                  { prevPage :: Maybe String
                  , photos :: Array
                                { width :: Int
                                , url :: String
                                , id :: Int
                                , height :: Int
                                }
                  , perPage :: Int
                  , page :: Int
                  , nextPage :: Maybe String
                  }

getCuratedResultfromJson = collect
  { nextPage: optionalField "next_page" (Just <$> string)
  , prevPage: optionalField "prev_page" (Just <$> string)
  , page: field "page" int
  , perPage: field "per_page" int
  , photos: field "photos" $ arrayOf getPhotosfromJson
}

getPhotosfromJson :: forall t77 t78.
  Monad t78 => Validation t78
                 (Array
                    (Variant
                       ( jsError :: { path :: List String
                                    , msg :: String
                                    }
                       | t77
                       )
                    )
                 )
                 Json
                 { width :: Int
                 , url :: String
                 , id :: Int
                 , height :: Int
                 }
getPhotosfromJson = collect
  { id: field "id" int
  ,  width: field "width" int
  , height: field  "height" int
  , url: field "url" string
  }
