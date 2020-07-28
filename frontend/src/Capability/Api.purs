module Capability.Api where

import Prelude
import Affjax (Error, get, post, printError)
import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import DTOs as DTO
import DTOs (Book)
import Data.Maybe (Maybe(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (MultipleErrors, renderForeignError)
import Network.RemoteData as RD
import Simple.JSON (class ReadForeign, readJSON, writeJSON)
import Affjax.RequestBody as RequestBody

searchBooks :: String -> Aff (RD.RemoteData ApiError (Array Book))
searchBooks title = (get ResponseFormat.string $ "/api/book/" <> title) >>= parseResp

fetchFeed' :: String -> Int -> Int -> Aff (RD.RemoteData ApiError DTO.FeedResponse)
fetchFeed' endpoint limit offset = do
  res <- post ResponseFormat.string endpoint (Just $ RequestBody.String $ writeJSON body)
  parseResp res
  where
  body :: DTO.FeedRequest
  body = { _offset: offset, _limit: limit }

fetchFeed = fetchFeed' "/api/feed"

fetchFeedRequests = fetchFeed' "/api/feed/requests"

fetchFeedRecommendations = fetchFeed' "/api/feed/recommendations"

fetchRequest :: String -> Aff (RD.RemoteData ApiError DTO.RequestPageResponse)
fetchRequest uuid = do
  res <- get ResponseFormat.string $ "/api/request/" <> uuid
  parseResp res

submitRequest :: String -> Aff (RD.RemoteData ApiError DTO.RequestResponse)
submitRequest bookUuid = do
  res <- post ResponseFormat.string ("/api/request") (Just $ RequestBody.String $ writeJSON body)
  parseResp res
  where
  body :: DTO.SubmitRecommendationRequest
  body = { _requestBookUuid: bookUuid }

-- TODO: discard return value
submitRecommendation :: String -> String -> Aff (RD.RemoteData ApiError DTO.Recommendation)
submitRecommendation requestUuid bookUuid = do
  res <- post ResponseFormat.string ("/api/request/" <> requestUuid) (Just $ RequestBody.String $ writeJSON body)
  parseResp res
  where
  body :: DTO.SubmitRecommendation
  body = { _recommendationBookUuid: bookUuid }

parseResp ::
  forall a c.
  ReadForeign c => Either Error { body :: String | a } -> Aff (RD.RemoteData ApiError c)
parseResp res = case lmap transformAjaxError res >>= (lmap transformJsonError <<< readJSON <<< _.body) of
  Right (dto) -> do
    {-- log $ show res --}
    pure $ RD.Success dto
  Left e -> do
    liftEffect $ log $ show e
    pure $ RD.Failure e

transformAjaxError :: Error -> ApiError
transformAjaxError err = NetworkError err

transformArgonautError :: String -> ApiError
transformArgonautError errors = ArgonautError errors

transformJsonError :: MultipleErrors -> ApiError
transformJsonError errors = DecodeError errors

data ApiError
  = DecodeError MultipleErrors
  | NetworkError Affjax.Error
  | ArgonautError String

instance showStringError :: Show ApiError where
  show (DecodeError errors) = show $ map renderForeignError errors
  show (NetworkError e) = printError e
  show (ArgonautError e) = e
