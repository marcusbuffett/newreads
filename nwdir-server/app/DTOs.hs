{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module DTOs where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as T
import Data.Time.Clock
import GHC.Generics
import Language.PureScript.Bridge

data RecRequest
  = RecRequest
      { book :: Book,
        additionalInfo :: Text,
        user :: User,
        created :: UTCTime,
        uuid :: Text
      }
  deriving (Generic)

instance ToJSON RecRequest

data User
  = User
      { uuid :: Text,
        username :: Text
      }
  deriving (Generic)

instance ToJSON User

data Book
  = Book
      { title :: Text,
        description :: Text,
        uuid :: Text,
        author :: Text,
        amazonLink :: Text,
        numRatings :: Int
      }
  deriving (Generic)

instance ToJSON Book

data Recommendation
  = Recommendation
      { book :: Book,
        user :: User,
        request :: RecRequest,
        uuid :: Text,
        leaves :: Int,
        created :: UTCTime
      }
  deriving (Generic)

instance ToJSON Recommendation

data RequestEndpointDTO
  = RequestEndpointDTO
      { request :: RecRequest,
        recommendations :: [Recommendation]
      }
  deriving (Generic)

instance ToJSON RequestEndpointDTO

data RequestsResponse
  = RequestsResponse
      { requests :: [RecRequest]
      }
  deriving (Generic)

instance ToJSON RequestsResponse

data HomeResponse
  = HomeResponse
      { requests :: [RecRequest],
        recommendations :: [Recommendation],
        offset :: Int
      }
  deriving (Generic)

instance ToJSON HomeResponse

data SubmitRecommendationRequest
  = SubmitRecommendationRequest
      { _recommendationBookUuid :: Text
      }
  deriving (Generic)

instance FromJSON SubmitRecommendationRequest

makeLensesWith abbreviatedFields ''SubmitRecommendationRequest

data SubmitRequestRequest
  = SubmitRequestRequest
      { _requestBookUuid :: Text
      }
  deriving (Generic)

instance FromJSON SubmitRequestRequest

makeLensesWith abbreviatedFields ''SubmitRequestRequest

data FeedRequestRequest
  = FeedRequestRequest
      { _offset :: Int,
        _limit :: Int
      }
  deriving (Generic)

instance FromJSON FeedRequestRequest

makeLensesWith abbreviatedFields ''FeedRequestRequest

data SubmitRequestResponse
  = SubmitRequestResponse
      { _requestUuid :: Text
      }
  deriving (Generic)

instance ToJSON SubmitRequestResponse

-- makeLensesWith abbreviatedFields ''SubmitRequestResponse

myTypes =
  [ mkSumType (Proxy :: Proxy Book),
    mkSumType (Proxy :: Proxy RequestsResponse),
    mkSumType (Proxy :: Proxy RecRequest)
  ]

createDTOs :: IO ()
createDTOs =
  writePSTypes
    "../frontend-halogen/src/Data"
    (buildBridge defaultBridge)
    myTypes
