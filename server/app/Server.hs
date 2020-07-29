{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import BookSearch as BookSearch
import Control.Lens hiding (like)
import Control.Monad.Trans.Class
import qualified DTOs as DTO
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.SearchEngine (SearchEngine, insertDocs, invariant)
import qualified Data.SearchEngine as SE
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Data.UUID as UUID
import Database.Selda
import qualified Database.Selda as S
import Database.Selda.PostgreSQL
import qualified Models as M
import Network.HTTP.Types.Status
import Network.URI.Encode (encodeText)
import Network.Wai (remoteHost)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.Environment (lookupEnv)
import UsernameGenerator (genUsername)
import Web.Scotty.Trans

server :: IO ()
server = do
  port <- lookupEnv "PORT"
  putStrLn "Constructing in-memory search engine..."
  searchEngine <- generateSearchEngine
  let config = Config {configSearchEngine = Just searchEngine}
  scottyT (maybe 4202 read port) M.withPersist (router config)

generateSearchEngine :: IO BookSearchEngine
generateSearchEngine = M.withPersist $ do
  booksMax <- liftIO (maybe 10000000 read <$> lookupEnv "SEARCH_MAX")
  loopInsertChunk 0 initialBookSearchEngine booksMax
  where
    loopInsertChunk start searchEngine booksMax = do
      t <- liftIO getCurrentTime
      allBooks <- query $ S.limit start chunkSize $ do
        books <- select M.goodreadsBooks
        order (books ! #_gbNumRatings) descending
        S.restrict (books ! #_gbFilled .== S.true)
        pure books
      -- let counts =
      -- take 100 $ sortBy (\(x, occ) (y, occ1) -> compare occ1 occ) $ filter (\(x, occ) -> occ > 1000) $ map (\x -> ([head x], length x)) . group . sort $ concatMap (extractTerms . M._gbTitle) allBooks
      -- liftIO $ print counts
      case (allBooks, (start + chunkSize > booksMax)) of
        ([], _) -> do
          pure searchEngine
        (_, True) -> do
          pure searchEngine
        (allBooks, _) -> do
          t <- liftIO getCurrentTime
          let docs = id $! map createBookDoc allBooks
          liftIO $ print $ length docs
          let searchEngine' = insertDocs docs searchEngine
          let result = SE.query searchEngine' ["har"]
          liftIO $ putStrLn $ "Search result is: " ++ show (head result)
          liftIO $ putStrLn $ "Total books added: " ++ show (start + chunkSize)
          t' <- liftIO getCurrentTime
          liftIO $ putStrLn ("Time to add " ++ show chunkSize ++ ": " ++ show (diffUTCTime t' t))
          loopInsertChunk (start + chunkSize) searchEngine' booksMax
    chunkSize = 5000
    createBookDoc :: M.GoodreadsBook -> BookSearch.BookDoc
    createBookDoc bookModel =
      BookSearch.BookDoc
        { docTitle = bookModel ^. M.title,
          docAuthor = bookModel ^. M.author,
          docUuid = bookModel ^. M.uuid,
          docNumRatings = bookModel ^. M.numRatings
        }

router :: Config -> ScottyT TL.Text (SeldaT PG IO) ()
router config = do
  middleware simpleCors
  middleware logStdoutDev
  get "/api/book/:query" $ searchBook config
  post "/api/feed" $ feed True True
  post "/api/feed/recommendations" $ feed True False
  post "/api/feed/requests" $ feed False True
  get "/api/request/:requestUuid" recRequest
  post "/api/recommendation/:recommendationUuid/click" trackClickthrough
  post "/api/request/:requestUuid" receiveRecommendation
  post "/api/request" receiveRequest
  get "/health" healthEndpoint

type EndpointBase a = ActionT TL.Text (SeldaT PG IO) a

type Endpoint = EndpointBase ()

liftSql ::
  SeldaT PG IO a ->
  EndpointBase a
liftSql = lift

-- liftReader :: ReaderT Config (SeldaT PG IO) a -> EndpointBase a
-- liftReader = lift

data Config
  = Config
      { configSearchEngine :: Maybe BookSearch.BookSearchEngine
      }

healthEndpoint :: Endpoint
healthEndpoint = do
  status status200

receiveRecommendation :: Endpoint
receiveRecommendation = do
  requestUuid :: Text <- param "requestUuid"
  request :: DTO.SubmitRecommendationRequest <- jsonData
  existingRecommendation <-
    liftSql $
      query
        ( do
            rec <- select M.recommendations
            restrict (rec ! #_rBookUuid .== S.text (DTO._recommendationBookUuid request))
            restrict (rec ! #_rRequestUuid .== S.text requestUuid)
            selectRecsRelated rec
        )
  case existingRecommendation of
    (recommendation : _) -> json . transformResponse $ recommendation
    [] -> do
      now <- liftIO $ getCurrentTime
      u <- UUID.toText <$> newUuid
      userId <- UUID.toText <$> newUuid
      liftSql $
        insert_
          M.users
          [ M.User
              { M._uUuid = userId,
                M._uUsername = "Mock user"
              }
          ]
      let rec =
            M.Recommendation
              { _rUuid = u,
                _rBookUuid = request ^. DTO.bookUuid,
                _rProfileUuid = userId,
                _rRequestUuid = requestUuid,
                _rCreated = now
              }
      liftSql $
        insert_
          M.recommendations
          [rec]
      json . transformResponse . head
        =<< liftSql
          ( query
              ( do
                  rec <- select M.recommendations
                  restrict (rec ! #_rUuid .== S.text u)
                  selectRecsRelated rec
              )
          )

receiveRequest :: Endpoint
receiveRequest = do
  request :: DTO.SubmitRequestRequest <- jsonData
  existingRequest <-
    liftSql $
      query
        ( do
            recReq <- select M.recRequests
            restrict (recReq ! #_rrBookUuid .== S.text (DTO._requestBookUuid request))
            pure recReq
        )
  case existingRequest of
    (request : _) -> do
      json $
        DTO.SubmitRequestResponse
          { _requestUuid = request ^. M.uuid
          }
    [] -> do
      now <- liftIO $ getCurrentTime
      u <- UUID.toText <$> newUuid
      userId <- UUID.toText <$> newUuid
      liftSql $
        insert_
          M.users
          [ M.User
              { M._uUuid = userId,
                M._uUsername = "Mock user"
              }
          ]
      liftSql $
        insert_
          M.recRequests
          [ M.RecRequest
              { M._rrUuid = u,
                -- TODO: figure out how to have json
                M._rrBookUuid = request ^. DTO.bookUuid,
                M._rrProfileUuid = userId,
                M._rrAdditionalInfo = "",
                M._rrCreated = now
              }
          ]
      json $
        DTO.SubmitRequestResponse
          { _requestUuid = u
          }

trackClickthrough :: Endpoint
trackClickthrough = do
  createClickthrough
  status status200
  where
    createClickthrough = do
      recommendationUuid :: Text <- param "recommendationUuid"
      u <- UUID.toText <$> newUuid
      req <- request
      let ip = T.pack $ show $ remoteHost req
      -- ip :: Text <- (request >>= T.pack . show . remoteHost)
      liftSql $
        insert_
          M.recommendationClickthroughs
          [ M.RecommendationClickthrough
              { _rcUuid = u,
                _rcIpAddress = ip,
                _rcRecommendationUuid = recommendationUuid
              }
          ]

recRequest :: Endpoint
recRequest = do
  requestUuid :: Text <- param "requestUuid"
  res <-
    liftSql $ query $ do
      requests <- select M.recRequests
      book <- select M.goodreadsBooks
      user <- select M.users
      restrict (requests ! #_rrUuid .== S.text requestUuid)
      restrict (book ! #_gbUuid .== requests ! #_rrBookUuid)
      restrict (user ! #_uUuid .== requests ! #_rrProfileUuid)
      return (requests :*: book :*: user)
  -- let gen = mkUsersGen requestUuid
  -- let [requestUsername, recUsernames] = genUsernames gen
  let requests = map createDTO res
  recDTOs <- liftSql $ getRecs requestUuid
  case requests of
    (requestDTO : _) -> do
      json $ createRequestEndpointDTO requestDTO recDTOs
    [] -> status status404
  where
    createDTO res = toRecRequestDTO (first res) (second res) (third res)
    createRequestEndpointDTO req recs = DTO.RequestEndpointDTO {request = req, recommendations = recs}

getRecs :: Text -> SeldaT PG IO [DTO.Recommendation]
getRecs reqUuid =
  map transformResponse
    <$> query
      ( do
          recs <- select M.recommendations
          restrict (recs ! #_rRequestUuid .== S.text reqUuid)
          selectRecsRelated recs
      )

-- selectRecsRelated :: S.Row PG M.Recommendation -> (M.User :*: M.GoodreadsBook :*: M.RecRequest :*: M.User :*: M.GoodreadsBook :*: M.Recommendation)
selectRecsRelated recs = do
  order (recs ! #_rCreated) descending
  req <- select M.recRequests
  restrict (req ! #_rrUuid .== recs ! #_rRequestUuid)
  recBook <- select M.goodreadsBooks
  restrict (recBook ! #_gbUuid .== recs ! #_rBookUuid)
  reqBook <- select M.goodreadsBooks
  restrict (reqBook ! #_gbUuid .== req ! #_rrBookUuid)
  recUser <- select M.users
  restrict (recUser ! #_uUuid .== recs ! #_rProfileUuid)
  reqUser <- select M.users
  restrict (reqUser ! #_uUuid .== req ! #_rrProfileUuid)
  -- num_leaves <- aggregateNumLeaves (recs ! #_rrUuid)
  -- restrict (leavesRecUuid .== S.text (recommendation ^. M.uuid))
  -- restrict (leavesRecUuid .== S.text (T.pack (recommendation ^. M.uuid)))
  return (recUser :*: recBook :*: req :*: reqUser :*: reqBook :*: recs)

searchBook :: Config -> Endpoint
searchBook config = case configSearchEngine config of
  Nothing -> status status404
  Just searchEngine -> do
    queryParam :: String <- param "query"
    let terms = filterSearchTerms $ T.words $ T.toLower $ T.pack queryParam
    let bookUuids = SE.query searchEngine terms
    -- liftIO $ print $ length bookUuids
    -- liftIO $ print $ bookUuids
    -- let results = SE.queryExplain searchEngine terms
    -- liftIO $
    -- mapM_
    -- ( \(explanation, bookUuid) -> do
    -- -- liftIO $ print $ "result uuid is " ++ T.unpack bookUuid
    -- book <-
    -- head
    -- <$> ( M.withPersist $
    -- query
    -- ( do
    -- books <- select M.goodreadsBooks
    -- restrict (books ! #_gbUuid .== S.text bookUuid)
    -- pure books
    -- )
    -- )
    -- liftIO $ print $ (show $ M._gbNumRatings book) ++ " ratings for " ++ (T.unpack $ M._gbTitle book)
    -- liftIO $ print $ "explanation is " ++ show explanation
    -- )
    -- results
    books <- liftSql $ query $ do
      books <- select M.goodreadsBooks
      restrict (books ! #_gbUuid `isIn` map S.text bookUuids)
      pure books
    let booksLookup = Map.fromList (map (\x -> (M._gbUuid x, x)) books)
    json (toBookDTO <$> mapMaybe (`Map.lookup` booksLookup) bookUuids)
    where
      filterSearchTerms = filter (/= T.pack "by")

feed :: Bool -> Bool -> Endpoint
feed includeRecs includeRequests = do
  request :: DTO.FeedRequestRequest <- jsonData
  let limit = DTO._limit request
  let offset = DTO._offset request
  requests <- if includeRequests then liftSql (feedRequests offset limit) else pure []
  recs <- if includeRecs then liftSql (feedRecommendations offset limit) else pure []
  let newOffset = case (includeRecs, includeRequests) of
        (True, True) -> min (length requests) (length recs)
        (False, True) -> length requests
        (True, False) -> length recs
        _ -> undefined
  json $
    DTO.HomeResponse
      { requests = requests,
        recommendations = recs,
        offset = offset + newOffset
      }

feedRequests :: Int -> Int -> SeldaT PG IO [DTO.RecRequest]
feedRequests offset limit = do
  res <-
    query $ do
      requests <- S.limit offset limit $ select M.recRequests
      order (requests ! #_rrCreated) descending
      -- let requests = limit 0 1 requests
      book <- select M.goodreadsBooks
      user <- select M.users
      restrict (book ! #_gbUuid .== requests ! #_rrBookUuid)
      restrict (user ! #_uUuid .== requests ! #_rrProfileUuid)
      return (requests :*: book :*: user)
  pure $ map createDTO res
  where
    createDTO res = toRecRequestDTO (first res) (second res) (third res)

feedRecommendations :: Int -> Int -> SeldaT PG IO [DTO.Recommendation]
feedRecommendations offset limit = do
  let q =
        query $ do
          recs <- S.limit offset limit $ select M.recommendations
          selectRecsRelated recs
  q >>= \x -> pure (map transformResponse x)

transformResponse :: (M.User :*: M.GoodreadsBook :*: M.RecRequest :*: M.User :*: M.GoodreadsBook :*: M.Recommendation) -> DTO.Recommendation
transformResponse (recUser :*: recBook :*: req :*: reqUser :*: reqBook :*: rec) =
  DTO.Recommendation
    { book = toBookDTO recBook,
      user = toUserDTO recUser mockUsername,
      request = toRecRequestDTO req reqBook reqUser,
      uuid = rec ^. M.uuid,
      leaves = 0,
      created = rec ^. M.created
    }
  where
    mockUsername = genUsername (req ^. M.uuid) (Just $ rec ^. M.uuid)

aggregateNumLeaves :: Text -> S.Query _ (S.Col PG Int)
aggregateNumLeaves recUuid = aggregate $ do
  rc <- select M.recommendationClickthroughs
  restrict (rc ! #_rcRecommendationUuid .== S.text recUuid)
  return (count (rc ! #_rcRecommendationUuid))

toRecRequestDTO :: M.RecRequest -> M.GoodreadsBook -> M.User -> DTO.RecRequest
toRecRequestDTO r b u =
  DTO.RecRequest
    { book = toBookDTO b,
      additionalInfo = r ^. M.additionalInfo,
      user = toUserDTO u mockUsername,
      created = r ^. M.created,
      uuid = r ^. M.uuid
    }
  where
    mockUsername = genUsername (r ^. M.uuid) Nothing

toBookDTO :: M.GoodreadsBook -> DTO.Book
toBookDTO book =
  DTO.Book
    { title = book ^. M.title,
      description = book ^. M.description,
      uuid = book ^. M.uuid,
      author = book ^. M.author,
      amazonLink = generateAmazonLink book,
      numRatings = book ^. M.numRatings
    }

generateAmazonLink :: M.GoodreadsBook -> Text
generateAmazonLink book = "https://amazon.com/" <> linkPath (book ^. M.isbn) (book ^. M.title)
  where
    linkPath :: Maybe Text -> Text -> Text
    linkPath (Just isbn) _ = "gp/product/" <> isbn <> "?tag=" <> associatesTag
    linkPath Nothing title = "s?k=" <> encodeText title <> "&i=stripbooks&tag=" <> associatesTag
    associatesTag = "mbuffett-20"

toUserDTO :: M.User -> Text -> DTO.User
toUserDTO u name =
  DTO.User {uuid = u ^. M.uuid, username = name}

selectBookQuery :: String -> Query s (Row s M.GoodreadsBook)
selectBookQuery t = do
  book <- select M.goodreadsBooks
  restrict ((book ! #_gbTitle) `like` S.text (T.pack $ "%" ++ t ++ "%"))
  return book
