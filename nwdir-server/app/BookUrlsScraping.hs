{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module BookUrlsScraping where

-- import qualified Filesystem.Path as Path
-- import Filesystem.Path ((</>))

-- import qualified Filesystem.Path as Path
-- import Filesystem.Path ((</>))

import qualified Codec.Compression.GZip as GZip
import Control.Applicative ((<|>))
import Control.Concurrent
import qualified Control.Concurrent.PooledIO.InOrder as PooledInOrder
import qualified Control.Concurrent.PooledIO.Independent as Pooled
import Control.Exception as Exception
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
-- import qualified Data.ByteString as B
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Time.Clock
import Data.Tree.NTree.TypeDefs
import qualified Data.UUID as UUID
import Data.UUID.V4
import Data.Void
import qualified Database.Selda as Selda
import Database.Selda ((!), (.<=), (.==))
import Debug.Trace
import MockBooks
import qualified Models
import Network.HTTP.Client (HttpException)
import Network.HTTP.Conduit
import Network.URI
import qualified Network.Wreq as Wreq
import Safe
import System.Directory
import System.Environment (lookupEnv)
import qualified System.FilePath.Posix as Filepath
import System.IO
import System.IO.Error
import System.ProgressBar
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import System.Time.Utils (renderSecs)
import Text.HTML.Scalpel
import qualified Text.HTML.Scalpel as Scalpel
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.RE.TDFA.String
import qualified Text.XML.HXT.Core as X
import Text.XML.HXT.Core ((<<<), (>>>))
import Prelude hiding (asin)

firstJust [] = Nothing
firstJust ((Just x) : _) = Just x
firstJust ((Nothing) : xs) = firstJust xs

microToSeconds = (*) 1000000

sitemapsDirectory :: Text
sitemapsDirectory = "data/sitemaps"

downloadSitemaps :: IO ()
downloadSitemaps = do
  threads <- getNumCapabilities
  Pooled.runLimited threads $ map saveUrl sitemaps

sitemaps :: [(Text, Int)]
sitemaps = map getSitemap [1 .. 400]
  where
    getSitemap i = (getSitemapUrl i, i)
    getSitemapUrl :: Int -> Text
    getSitemapUrl i = "https://www.goodreads.com/sitemap." <> (T.pack . show) i <> ".xml.gz"

saveUrl :: (Text, Int) -> IO ()
saveUrl (url, i) = do
  putStrLn $ "Downloading sitemap " ++ show i
  doc <- simpleHttp (T.unpack url)
  let path = sitemapFilepath i
  createDirectoryIfMissing True (Filepath.takeDirectory (T.unpack path))
  B.writeFile (T.unpack path) doc

sitemapFilepath :: Int -> Text
sitemapFilepath i = T.pack $ Filepath.joinPath $ map T.unpack [sitemapsDirectory, pad 5 i <> "_sitemap.txt"]

pad :: Int -> Int -> Text
pad l n =
  if l > length (show n)
    then "0" <> pad (l - 1) n
    else T.pack $ show n

getBookUrls :: IO ()
getBookUrls = do
  let fs = map sitemapFilepath [1 .. 400]
  TIO.writeFile "data/bookurls.txt" ""
  urls <- mapM getUrlsOneFile fs
  return ()
  where
    getUrlsOneFile f = do
      b <- TIO.readFile (T.unpack f)
      let urls = findBookUrls b
      -- return urls
      TIO.appendFile "data/bookurls.txt" $ T.intercalate "\n" urls

-- parseBookUrls :: NTree X.XNode -> Text
-- works
-- parseBookUrls = X.deep (X.hasName "loc") >>> X.getText

-- ~30% faster
findBookUrls :: T.Text -> [T.Text]
findBookUrls = map getUrlFromLine . filter (T.isInfixOf "/book/") . T.lines
  where
    getUrlFromLine = T.replace "</loc>" "" . T.replace "<loc>" "" . T.strip

-- Section: List scraping

data ScrapedBook
  = ScrapedBook
      { title :: Text,
        author :: Text,
        numGoodreadsRatings :: Int,
        goodreadsId :: Text,
        imageURL :: Maybe Text,
        isbn :: Maybe Text,
        asin :: Maybe Text,
        description :: Maybe Text,
        goodreadsPath :: Text
      }
  deriving (Show)

mostPopularListId = "1.Best_Books_Ever"

popularList = ListPage mostPopularListId 1 526

data ListPage = ListPage Text Int Int deriving (Show)

nextPage (ListPage id n y) = ListPage id (n + 1) y

pageMax (ListPage id n y) = y

firstPage id = ListPage id 1

scrapePopularList = do
  liftIO $ putStrLn "Scraping popular list!"
  Models.withPersist $ do
    pb <- liftIO $ newProgressBar absProgresStyle 10 (Progress 0 (pageMax popularList) ())
    let pages = map (\n -> ListPage mostPopularListId n (pageMax popularList)) [1 .. (pageMax popularList)]
    liftIO $ Pooled.run $ map (\p -> saveBooksFromPage p pb) pages
  where
    saveBooksFromPage :: ListPage -> ProgressBar () -> IO ()
    saveBooksFromPage b@(ListPage _ pageIndex _) pb = do
      books <- scrapePage b
      liftIO $ incProgress pb 1
      -- liftIO $ putStrLn $ "Scraped page" ++ show pageIndex
      bookModels <- mapM (\b -> ((T.pack . UUID.toString) <$> nextRandom) >>= \u -> pure $ scrapedToModel b u 0 False) books
      mapM (\book -> Models.withPersist $ Selda.tryInsert Models.goodreadsBooks [book]) bookModels
      pure ()

bookUrl id = "https://www.goodreads.com/book/show/" <> id

addGoodreadsDomain path = "https://www.goodreads.com" <> path

data ScrapedDetailPage
  = ScrapedDetailPage
      { _sdBook :: ScrapedBook,
        _sdRelatedBookPaths :: [Text]
      }
  deriving (Show)

repeatM = sequence . repeat

absProgresStyle = defStyle {stylePostfix = exact <> " - " <> remainingLabel}
  where
    remainingLabel = remainingTime (TL.pack . renderSecs . round) " estimating time remaining..."

recursiveScrape :: IO ()
recursiveScrape = do
  -- TODO: for each depth, call scrapeBookDetailsPages with new depth
  let depthGoal = 2
  mapM_ scrapeAtDepth [0 .. depthGoal]

scrapeAtDepth :: Int -> IO ()
scrapeAtDepth depth = do
  books <- Models.withPersist $ Selda.query $ do
    books <- Selda.select Models.goodreadsBooks
    Selda.restrict (books ! #_gbDepth .== Selda.int depth)
    Selda.restrict (books ! #_gbFilled .== Selda.false)
    pure books
  rng <- newStdGen
  case books of
    [] -> print $ "No books at depth: " <> show depth
    books -> do
      let shuffledBooks = shuffle' books (length books) rng
      scrapeBookDetailsPages ((T.unpack . (^. Models.goodreadsPath)) <$> shuffledBooks) depth

scrapeBookDetailsPages :: [String] -> Int -> IO ()
scrapeBookDetailsPages [] _ = pure ()
scrapeBookDetailsPages bookPaths depth = do
  threads <- getNumCapabilities
  -- pb <-
  -- newProgressBar
  -- absProgresStyle
  -- 10
  -- (Progress 0 (length bookPaths) ())
  -- let capabilities = threads
  let capabilities = 4
  putStrLn $ "Will scrape " <> (show $ length bookPaths) <> " books with " <> show capabilities <> " capabilities at depth " <> show depth
  Pooled.runLimited capabilities $ map (\id -> detailScrapeBook id depth) bookPaths
  where
    detailScrapeBook :: String -> Int -> IO ()
    detailScrapeBook bookId depth = do
      Models.withPersist $ innerScrape bookId depth
    handleExc :: HttpException -> IO (Maybe ScrapedDetailPage)
    handleExc e = do
      print "Caught"
      print e
      case e of
        (HttpExceptionRequest _ (StatusCodeException response rb)) -> do
          print response
          print "---"
          print rb
          print $ responseBody response
          print "!!!"
        _ -> pure ()
      pure Nothing
    innerScrape :: (MonadIO m, Selda.MonadMask m, Selda.MonadSelda m) => String -> Int -> m ()
    innerScrape bookPath depth = do
      -- TODO: restrict to books that aren't filled
      details <- liftIO $ catch (liftIO $ scrapeBookPage $ T.pack bookPath) (liftIO . handleExc)
      case details of
        Just details -> do
          uuid <- liftIO $ (T.pack . UUID.toString) <$> nextRandom
          let newModel = scrapedToModel (_sdBook details) uuid depth True
          Selda.deleteFrom Models.goodreadsBooks (\b -> (b ! #_gbGoodreadsPath) .== Selda.text (T.pack bookPath))
          Selda.deleteFrom Models.goodreadsBooks (\b -> (b ! #_gbGoodreadsId) .== Selda.text (newModel ^. Models.goodreadsId))
          Selda.insert_ Models.goodreadsBooks [newModel]
          -- liftIO $ incProgress pb 1
          liftIO $ putStr "."
          liftIO $ hFlush stdout
          let newDepth = depth + 1
          relatedModels <- liftIO $ mapM (createRelatedBookModel newDepth) $ _sdRelatedBookPaths details
          mapM_ (Selda.tryInsert Models.goodreadsBooks) $ map (\x -> [x]) relatedModels
        Nothing -> do
          liftIO $ print $ "Failed to parse for book: " <> bookPath
    createRelatedBookModel :: Int -> Text -> IO Models.GoodreadsBook
    createRelatedBookModel depth url = do
      uuid <- nextRandom
      let id = fromMaybe "book id not found" (parseMaybe parseBookUrl url)
      pure $
        Models.GoodreadsBook
          { Models._gbTitle = "",
            Models._gbUuid = T.pack $ UUID.toString uuid,
            Models._gbAuthor = "",
            Models._gbIsbn = mempty,
            Models._gbAsin = mempty,
            Models._gbGoodreadsId = id,
            Models._gbNumRatings = 0,
            Models._gbImageURL = mempty,
            Models._gbDepth = depth,
            Models._gbFilled = False,
            Models._gbGoodreadsPath = url,
            Models._gbDescription = mempty
          }
    forceInsert ::
      (MonadIO m, Selda.MonadMask m, Selda.MonadSelda m) =>
      Models.GoodreadsBook ->
      m ()
    forceInsert book = do
      let grId = book ^. Models.goodreadsId
      liftIO $ print $ "Deleting book with goodreads id: " ++ show grId
      Selda.deleteFrom Models.goodreadsBooks (\b -> (b ! #_gbGoodreadsId) .== Selda.text grId)
      Selda.insert_ Models.goodreadsBooks [book]

getLowerResolutionUrl imageUrl = T.replace ".jpg" "._SX100_.jpg" imageUrl

getBookHTML :: IO ()
getBookHTML = do
  print wreqOpts
  r <- Wreq.getWith wreqOpts "https://www.goodreads.com/book/show/2767052-the-hunger-games"
  case r ^? Wreq.responseBody of
    Just b -> do
      let body = (decodeUtf8 . B.toStrict) b
      -- TIO.writeFile (T.unpack $ "debug_" <> ".response") body
      print $ "IP: " <> show b
    Nothing -> do
      print $ "Failed to fetch ip: " <> (T.pack . show) (r ^. Wreq.responseStatus . Wreq.statusCode)

wreqOpts =
  Wreq.defaults
    -- & (Wreq.proxy ?~ Wreq.httpProxy "localhost" 8080)
    & (Wreq.header "User-Agent" .~ ["Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9"])
    & (Wreq.header "Host" .~ ["www.goodreads.com"])
    & (Wreq.header "Accept" .~ ["text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"])
    & (Wreq.header "Cookie" .~ ["csid=BAhJIhgyMTUtNTg1NjY4Mi00NjgzOTE3BjoGRVQ%3D--8501de8bbc220ae1cd537510968d7bdde27b3419; never_show_interstitial=true; p=49p1a4-RnlM8sdPtMRMPXkNI0tmy9IuDhZOpsvSdPFdh9OV3; u=bBB567Jx24aCbdUmN3IiAEgL_B9f6nUJQ2YPdravok27ordY; locale=en; _session_id2=c43f830be449ab4381a845d18339c4f4"])
    & (Wreq.header "Accept-Encoding" .~ ["gzip", "deflate"])
    & (Wreq.header "Accept-Language" .~ ["en-US,en;q=0.5"])
    & (Wreq.header "Connection" .~ ["keep-alive"])

convertBookPathToFile path cachePath = cachePath Filepath.</> "bookHtml" Filepath.</> (T.unpack $ (T.replace "/" "_" path) <> ".html.gz")

getBookPageHtml :: Text -> IO (Maybe Text)
getBookPageHtml bookPath = do
  cachePath <- fromMaybe "./" <$> lookupEnv "CACHE_PATH"
  let filePath = convertBookPathToFile bookPath cachePath
  cachedHtml <- getCachedBookPageHtml filePath
  createDirectoryIfMissing True $ Filepath.takeDirectory filePath
  case cachedHtml of
    Just html -> pure cachedHtml
    Nothing -> do
      let url = addGoodreadsDomain bookPath
      r <- Wreq.getWith wreqOpts (T.unpack url)
      case r ^? Wreq.responseBody of
        Just b -> do
          let body = (decodeUtf8 . B.toStrict) b
          B.writeFile filePath (GZip.compress $ B.fromStrict $ TextEncoding.encodeUtf8 body)
          pure $ Just body
        Nothing -> do
          print $ "Failed to fetch book html: " <> (T.pack . show) (r ^. Wreq.responseStatus . Wreq.statusCode)
          pure Nothing

getCachedBookPageHtml :: FilePath -> IO (Maybe Text)
getCachedBookPageHtml filePath = do
  contents :: Either IOException Text <- Exception.try $ TIO.readFile filePath
  case contents of
    Left _ -> pure Nothing
    Right contents -> do
      pure $ Just (TextEncoding.decodeUtf8 $ B.toStrict $ GZip.decompress $ B.fromStrict $ TextEncoding.encodeUtf8 contents)

scrapeBookPage :: Text -> IO (Maybe ScrapedDetailPage)
scrapeBookPage bookPath = do
  -- putStrLn $ "Scraping book page: " ++ (T.unpack $ book ^. Models.title)
  html <- getBookPageHtml bookPath
  case html of
    Just html -> pure $ scrapeStringLike html scrapeDetailPage
    Nothing -> pure Nothing
  where
    scrapeDetailPage :: Scraper Text ScrapedDetailPage
    scrapeDetailPage = do
      -- chroots ("div" @: [AttributeString "id" @= "all_votes"] // "tr") book
      b <- scrapeBook
      related <- scrapeRelated
      pure $
        ScrapedDetailPage
          { _sdBook = b,
            _sdRelatedBookPaths = related
          }
    scrapeRelated :: Scraper Text [Text]
    scrapeRelated = do
      chroots
        ( "div" @: [Scalpel.match (\k v -> "relatedWorks" `isInfixOf` v)]
            // "div" @: [hasClass "carouselRow"]
            // "ul"
            // "li"
        )
        scrapeRelatedUrl
    scrapeRelatedUrl :: Scraper Text Text
    scrapeRelatedUrl = do
      url <- attr "href" "a"
      let path = fromMaybe "book path not found" (parseMaybe parseBookUrlForPath url)
      return path
    scrapeBook :: Scraper Text ScrapedBook
    scrapeBook = do
      title <- text ("h1" @: [AttributeString "id" @= "bookTitle"])
      author <- text $ ("a" @: [hasClass "authorName"] // "span")
      descriptions <- innerHTMLs $ ("div" @: [AttributeString "id" @= "descriptionContainer"] // "div" // "span")
      let description = atMay descriptions 1 <|> atMay descriptions 0 <|> Nothing
      numRatingsString <- attr "content" ("meta" @: [AttributeString "itemprop" @= "ratingCount"])
      imageUrl <- attr "src" ("div" @: [hasClass "bookCoverPrimary"] // "img")
      bookLink <- attr "href" ("link" @: [AttributeString "rel" @= "canonical"])
      isbn <- (firstJust <$> chroots ("div" @: [AttributeString "id" @= "bookDataBox"] // "div") getIsbn)
      let isbnParsed = isbn >>= parseMaybe parseIsbn
      -- isbn <- attr "content" ("meta" @: [AttributeString "property" @= "books:isbn"])
      let goodreadsId = fromMaybe "book id not found" (parseMaybe parseBookUrl bookLink)
      let goodreadsPath = fromMaybe "book path not found" (parseMaybe parseBookUrlForPath bookLink)
      let numGoodreadsRatings = fromMaybe 0 (parseMaybe parseNumber numRatingsString)
      return $
        ScrapedBook
          { title = T.strip title,
            author = author,
            numGoodreadsRatings = numGoodreadsRatings,
            isbn = isbnParsed,
            -- should fill this in when getting asin, if we do
            asin = Nothing,
            goodreadsId = goodreadsId,
            goodreadsPath = goodreadsPath,
            imageURL = Just . getLowerResolutionUrl $ imageUrl,
            description = description
          }
    getIsbn :: Scraper Text (Maybe Text)
    getIsbn = do
      -- <- text anySelector
      infoBoxValue <- text ("div" @: [hasClass "infoBoxRowItem"])
      infoBoxTitle <- text ("div" @: [hasClass "infoBoxRowTitle"])
      pure $ case infoBoxTitle of
        "ISBN" -> Just infoBoxValue
        _ -> Nothing

scrapedToModel :: ScrapedBook -> Text -> Int -> Bool -> Models.GoodreadsBook
scrapedToModel b uuid d isFilled =
  Models.GoodreadsBook
    { Models._gbTitle = title b,
      Models._gbUuid = uuid,
      Models._gbAuthor = author b,
      Models._gbGoodreadsId = goodreadsId b,
      Models._gbIsbn = isbn b,
      Models._gbAsin = asin b,
      Models._gbNumRatings = numGoodreadsRatings b,
      Models._gbImageURL = imageURL b,
      Models._gbDepth = d,
      Models._gbFilled = isFilled,
      Models._gbGoodreadsPath = goodreadsPath b,
      Models._gbDescription = case description b of
        Just d -> d
        Nothing -> ""
    }

listPageURL (ListPage id n m) = "https://www.goodreads.com/list/show/" <> id <> "?page=" <> (T.pack $ show n)

scrapePage :: ListPage -> IO [ScrapedBook]
scrapePage page@(ListPage id n m) = do
  r <- Wreq.getWith wreqOpts (T.unpack $ listPageURL page)
  case r ^? Wreq.responseBody of
    Just b -> do
      let body = (decodeUtf8 . B.toStrict) b
      let bs = scrapeStringLike body books
      case bs of
        Just bs ->
          pure bs
        Nothing -> pure []
    Nothing -> do
      print $ "Failed to fetch ip: " <> (T.pack . show) (r ^. Wreq.responseStatus . Wreq.statusCode)
      pure []
  where
    books :: Scraper Text [ScrapedBook]
    books = chroots ("div" @: [AttributeString "id" @= "all_votes"] // "tr") book
    book :: Scraper Text ScrapedBook
    book = do
      title <- text $ ("a" @: [hasClass "bookTitle"] // "span")
      bookUrl <- attr "href" $ ("a" @: [hasClass "bookTitle"])
      author <- text $ ("a" @: [hasClass "authorName"] // "span")
      ratingsDescription <- text $ ("span" @: [hasClass "minirating"])
      let numGoodreadsRatings = fromMaybe 0 (parseMaybe parseRatingsDescription ratingsDescription)
      let goodreadsId = fromMaybe "book id not found" (parseMaybe parseBookUrl bookUrl)
      return $
        ScrapedBook
          { title = title,
            author = author,
            isbn = Nothing,
            asin = Nothing,
            numGoodreadsRatings = numGoodreadsRatings,
            goodreadsId = goodreadsId,
            imageURL = Nothing,
            description = Nothing,
            goodreadsPath = bookUrl
          }

parseRatingsDescription :: Parsec Void Text Int
parseRatingsDescription = do
  _ <- takeWhileP Nothing ((/=) '—')
  _ <- char '—'
  _ <- space1
  numRatingsString <- many (digitChar <|> char ',')
  let numRatings = (read $ filter (/= ',') numRatingsString :: Int)
  _ <- takeRest
  return numRatings

parseNumber :: Parsec Void Text Int
parseNumber = do
  numRatingsString <- many (digitChar <|> char ',')
  let numRatings = (read $ filter (/= ',') numRatingsString :: Int)
  _ <- takeRest
  return numRatings

parseBookUrl :: Parsec Void Text Text
parseBookUrl = do
  many $ satisfy (not . isDigit)
  digits <- many digitChar
  _ <- takeRest
  pure $ T.pack digits

parseBookUrlForPath :: Parsec Void Text Text
parseBookUrlForPath = do
  let one = chunk "goodreads.com" <|> (anySingle >> one) in one
  takeRest

parseIsbn :: Parsec Void Text Text
parseIsbn = do
  _ <- many spaceChar
  isbn <- many alphaNumChar
  _ <- takeRest
  pure $ T.pack isbn

-- pure ""

scrapeMockBooks = scrapeBookDetailsPages allMockBooks 0

scrapeOneMockBook = scrapeBookDetailsPages ["/book/show/" ++ deathlyhallows] 0
