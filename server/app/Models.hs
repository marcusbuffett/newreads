{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Models where

import Control.Lens
import Control.Monad (replicateM)
import Data.Aeson (ToJSON)
import qualified Data.Char as C
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.UUID as UUID
import Database.Selda
import qualified Database.Selda as S
import Database.Selda.PostgreSQL
import Debug.Trace
import MockBooks
import Safe
import System.Environment (lookupEnv)
import System.Random

data GoodreadsBook
  = GoodreadsBook
      { _gbTitle :: Text,
        _gbAuthor :: Text,
        _gbUuid :: Text,
        _gbGoodreadsId :: Text,
        _gbGoodreadsPath :: Text,
        _gbNumRatings :: Int,
        _gbImageURL :: Maybe Text,
        _gbIsbn :: Maybe Text,
        -- Not scraping this for now
        _gbAsin :: Maybe Text,
        _gbDepth :: Int,
        _gbFilled :: Bool,
        _gbDescription :: Text
      }
  deriving (Generic, Show)

instance SqlRow GoodreadsBook

instance ToJSON GoodreadsBook

makeLensesWith abbreviatedFields ''GoodreadsBook

data RecRequest
  = RecRequest
      { _rrBookUuid :: Text,
        _rrProfileUuid :: Text,
        _rrAdditionalInfo :: Text,
        _rrUuid :: Text,
        _rrCreated :: UTCTime
      }
  deriving (Generic, Show)

makeLensesWith abbreviatedFields ''RecRequest

instance SqlRow RecRequest

instance ToJSON RecRequest

data RecRequestProp
  = RecRequestProp
      { _rrpProp :: Text,
        _rrpDescription :: Text,
        _rrpUuid :: Text
      }
  deriving (Generic, Show)

instance SqlRow RecRequestProp

instance ToJSON RecRequestProp

makeLensesWith abbreviatedFields ''RecRequestProp

data RecRequestPropsJoin
  = RecRequestPropsJoin
      { _rrjPropUuid :: Text,
        _rrjRequestUuid :: Text
      }
  deriving (Generic)

instance SqlRow RecRequestPropsJoin

instance ToJSON RecRequestPropsJoin

makeLensesWith abbreviatedFields ''RecRequestPropsJoin

data RecommendationClickthrough
  = RecommendationClickthrough
      { _rcUuid :: Text,
        _rcRecommendationUuid :: Text,
        _rcIpAddress :: Text
      }
  deriving (Generic)

instance SqlRow RecommendationClickthrough

instance ToJSON RecommendationClickthrough

makeLensesWith abbreviatedFields ''RecommendationClickthrough

data Recommendation
  = Recommendation
      { _rUuid :: Text,
        _rBookUuid :: Text,
        _rProfileUuid :: Text,
        _rRequestUuid :: Text,
        _rCreated :: UTCTime
      }
  deriving (Generic, Show)

instance SqlRow Recommendation

instance ToJSON Recommendation

makeLensesWith abbreviatedFields ''Recommendation

data User
  = User
      { _uUuid :: Text,
        _uUsername :: Text
      }
  deriving (Generic)

instance SqlRow User

instance ToJSON User

makeLensesWith abbreviatedFields ''User

users :: Table User
users =
  tableFieldMod "users" [#_uUuid :- primary] stripUnderscores

stripUnderscores = T.dropWhile (\c -> c == '_' || C.isLower c)

goodreadsBooks :: Table GoodreadsBook
goodreadsBooks =
  tableFieldMod
    "goodreadsBooks"
    [ #_gbUuid :- primary,
      #_gbGoodreadsId :- unique
    ]
    stripUnderscores

recRequests :: Table RecRequest
recRequests = tableFieldMod "recRequests" [#_rrUuid :- primary, #_rrCreated :- S.index] stripUnderscores

recommendations :: Table Recommendation
recommendations = tableFieldMod "recommendations" [#_rUuid :- primary, #_rCreated :- S.index] stripUnderscores

recommendationClickthroughs :: Table RecommendationClickthrough
recommendationClickthroughs = tableFieldMod "recommendationClickthroughs" [#_rcUuid :- primary] stripUnderscores

recRequestProps :: Table RecRequestProp
recRequestProps = tableFieldMod "recRequestProps" [#_rrpUuid :- primary] stripUnderscores

recRequestPropsJoins :: Table RecRequestPropsJoin
recRequestPropsJoins =
  tableFieldMod "recRequestPropsJoins" [(#_rrjPropUuid :+ #_rrjRequestUuid) :- unique] stripUnderscores

withPersist :: (MonadIO m, MonadMask m) => SeldaT PG m a -> m a
withPersist x = do
  host <- liftIO (fromMaybe "0.0.0.0" <$> lookupEnv "DB_HOST")
  dbPort <- liftIO (maybe 4240 read <$> lookupEnv "DB_PORT")
  dbName <- liftIO (fromMaybe "nwreads" <$> lookupEnv "DB_NAME")
  liftIO $ print $ "Host " ++ show host
  liftIO $ print $ "DbPort " ++ show dbPort
  liftIO $ print $ "DbName " ++ show dbName
  withPostgreSQL
    ( (T.pack dbName `on` T.pack host) {pgUsername = Just "nwreads_server", pgPassword = Just "password", pgPort = dbPort}
    )
    x

initializeTable :: IO ()
initializeTable =
  withPersist $ do
    createTable goodreadsBooks
    createTable recRequests
    createTable recRequestProps
    createTable recRequestPropsJoins
    createTable users
    createTable recommendations
    createTable recommendationClickthroughs

-- NOTE: does not recreate books table
recreateTables :: IO ()
recreateTables =
  withPersist $ do
    tryDropTable recRequests
    tryDropTable recRequestProps
    tryDropTable recRequestPropsJoins
    tryDropTable users
    tryDropTable recommendations
    tryDropTable recommendationClickthroughs
    tryCreateTable goodreadsBooks
    createTable recRequests
    createTable recRequestProps
    createTable recRequestPropsJoins
    createTable users
    createTable recommendations
    createTable recommendationClickthroughs

recreateAllTables :: IO ()
recreateAllTables = do
  putStrLn "Recreating all tables"
  withPersist $ do
    tryDropTable goodreadsBooks
    liftIO recreateTables

recommend :: Text -> String -> [MockRec] -> SeldaT PG IO ()
recommend userId recId recs = do
  requestTime <- liftIO $ do
    daysAgo :: Integer <- getStdRandom (randomR (1, 4))
    let dAgo :: NominalDiffTime = fromIntegral daysAgo
    current <- getCurrentTime
    pure $ addUTCTime ((60 * 60 * 24 * dAgo) :: NominalDiffTime) current
  u <- UUID.toText <$> newUuid
  book <- head <$> getBook recId
  -- case book of
  -- Nothing -> error $ "no book with the id " <> show recId <> " exists"
  -- Just book -> do
  let rr =
        RecRequest
          { _rrUuid = u,
            _rrBookUuid = _gbUuid (book :: GoodreadsBook),
            _rrProfileUuid = userId,
            _rrAdditionalInfo = "I like this book because",
            _rrCreated = requestTime
          }
  insert_
    recRequests
    [rr]
  -- TODO: scrape
  mapM_ (createRecommendation requestTime u) recs
  where
    createRecommendation :: UTCTime -> Text -> MockRec -> SeldaT PG IO ()
    createRecommendation requestTime requestUuid (MockRec bookId clicks) = do
      -- print recquestTime
      now <- liftIO $ getCurrentTime
      let sinceRequest = diffUTCTime now requestTime
      randomDiff <- liftIO $ getStdRandom $ randomR (0, (fromRational . toRational) sinceRequest :: Float)
      let recTime = addUTCTime (- (fromRational . toRational $ randomDiff)) requestTime
      u <- UUID.toText <$> newUuid
      recBook <- head <$> getBook bookId
      createClickthroughs u clicks
      insert_
        recommendations
        [ Recommendation
            { _rUuid = u,
              _rBookUuid = recBook ^. uuid,
              _rProfileUuid = userId,
              _rRequestUuid = requestUuid,
              _rCreated = recTime
            }
        ]
    getBook :: String -> SeldaT PG IO [GoodreadsBook]
    getBook id = query $ do
      book <- select goodreadsBooks
      restrict (book ! #_gbGoodreadsId .== S.text (T.pack id))
      pure book

createClickthroughs :: Text -> Int -> SeldaT PG IO ()
createClickthroughs recUuid n = do
  let clickThroughs :: IO [RecommendationClickthrough] = replicateM n $ createClickthrough recUuid
  liftIO clickThroughs >>= insert_ recommendationClickthroughs

-- createClickthroughsNotWorky recUuid n = do
-- -- • Could not deduce (MonadIO [])
-- -- arising from a use of ‘createClickthrough’
-- -- from the context: MonadSelda m
-- -- bound by the inferred type of
-- -- createClickthroughsNotWorky :: MonadSelda m =>
-- -- String -> Int -> m ()
-- -- at /home/marcus/Documents/projects/nowwhatdoiread/nwdir-server/app/Models.hs:(253,1)-(254,82)
-- -- • In the first argument of ‘(>>=)’, namely
-- -- ‘createClickthrough recUuid’
-- insert_ recommendationClickthroughs $ createClickthrough recUuid >>= replicate n

createClickthrough :: Text -> IO RecommendationClickthrough
createClickthrough recUuid = do
  u <- UUID.toText <$> newUuid
  pure $
    RecommendationClickthrough
      { _rcUuid = u,
        _rcIpAddress = "127.0.0.1",
        _rcRecommendationUuid = recUuid
      }

-- TODO: random time in the past X days for each request, random time within X
-- hours later for each rec
-- TODO: random # of clicks on each book, from 5 to 120
-- TODO: identifier for fake data
-- TODO: create username
-- TODO:

data MockRec = MockRec String Int

createMockData :: IO ()
createMockData = withPersist $ do
  creator <- getCreator
  recommend creator exhalations [MockRec fifthscience 15, MockRec threebodyproblem 1]
  recommend creator hitchikersguide [MockRec biff 12, MockRec guardsguards 21]
  recommend
    creator
    wayofkings
    [ MockRec ragedragons 10,
      MockRec nameofthewind 23,
      MockRec furiesofcalderon 42
    ]
  recommend creator whywesleep [MockRec goodtogo 12, MockRec peakexpertise 18]
  recommend creator seveneves [MockRec hyperion 2, MockRec childrenoftime 3, MockRec martian 120]
  where
    getCreator = do
      u <- UUID.toText <$> newUuid
      insert_
        users
        [ User
            { _uUuid = u,
              _uUsername = "The Creator"
            }
        ]
      pure $ u

-- books <- query $ limit 0 20 $ do
-- books <- select goodreadsBooks
-- restrict (books ! #_gbFilled .== true)
-- pure books
-- mapM_ insertRecRequest books
-- liftIO $ print "Created mock recommendation requests"

-- insertRecRequest :: GoodreadsBook -> SeldaT PG IO ()
-- insertRecRequest book = do
-- uu <- newUuid
-- let date = UTCTime (fromGregorian 2019 1 1) 0
-- insert_
-- users
-- [ User
-- { _uUuid = UUID.toText uu,
-- _uUsername = "test username"
-- }
-- ]
-- let rr = RecRequest
-- { _rrUuid = UUID.toText uu,
-- _rrBookUuid = _gbUuid (book :: GoodreadsBook),
-- _rrProfileUuid = UUID.toText uu,
-- _rrAdditionalInfo = "I like this book because",
-- _rrCreated = date
-- }
-- insert_
-- recRequests
-- [rr]
-- ru <- newUuid
-- t <- liftIO getCurrentTime
-- insert_
-- recommendations
-- [ Recommendation
-- { _rUuid = UUID.toText ru,
-- _rBookUuid = book ^. uuid,
-- _rProfileUuid = UUID.toText uu,
-- _rRequestUuid = UUID.toText uu,
-- _rCreated = t
-- }
-- ]

clearTables :: IO ()
clearTables =
  withPersist $ do
    dropTable goodreadsBooks
    dropTable recRequests
    dropTable recRequestProps
    dropTable recRequestPropsJoins
