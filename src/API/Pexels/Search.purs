module API.Pexels.Search where

import Prelude

import Data.Either (Either)
import Data.FormURLEncoded (FormURLEncoded(..), fromArray)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))

newtype ApiKey = ApiKey String

type Request =
  { query :: String
  , page :: Int
  , perPage :: Int
  }

toUrlEncoded ∷ Request → FormURLEncoded
toUrlEncoded { query, page, perPage } = fromArray $
  [ Tuple "query" (Just query)
  , Tuple "per_page" (Just $ show perPage)
  , Tuple "page" (Just $ show page)
  ]

type Photo =
  { width :: Int
  , height :: Int
  , url :: String
  }

--     photographer: "Name",
--     src: {
--       original: "https://*.jpg",
--       large: "https://*.jpg",
--       large2x: "https://*.jpg",
--       medium: "https://*.jpg",
--       small: "https://*.jpg",
--       portrait: "https://*.jpg",
--       landscape: "https://*.jpg",
--       tiny: "https://*.jpg"
--     }, (NEXT PHOTOS)]
--   }

type Result =
  { totalResults ∷ Either String Int
  , nextPage ∷ Maybe String
  , prevPage ∷ Maybe String
  -- , photos ∷ Array Photo
  }

