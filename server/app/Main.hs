{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import BookUrlsScraping
import qualified Control.Exception as E
import Control.Monad.Except
import Control.Monad.Reader
import DTOs
import Data.Semigroup ((<>))
import GHC.IO.Encoding
import Models
import Options.Applicative
import Server

data Opts
  = Opts
      { downloadsitemaps :: Bool,
        parsesitemapurls :: Bool,
        scrapepopularlist :: Bool,
        scraperecursive :: Bool,
        startserver :: Bool,
        recreatetables :: Bool,
        recreatealltables :: Bool,
        createnewtables :: Bool,
        mockdata :: Bool,
        droptable :: Bool,
        createdtos :: Bool,
        scrapebookid :: Maybe String,
        scrapemockbooks :: Bool,
        scrapeonemockbook :: Bool
      }

type AppConfig = MonadReader Opts

data AppError
  = IOError E.IOException

newtype App a
  = App
      { runApp :: ReaderT Opts (ExceptT AppError IO) a
      }
  deriving
    ( Monad,
      Functor,
      Applicative,
      AppConfig,
      MonadIO,
      MonadError AppError
    )

main :: IO ()
main = runProgram =<< parseCLI

versionOption :: Parser (a -> a)
versionOption = infoOption "0.1" (long "version" <> help "Show version")

runProgram :: Opts -> IO ()
runProgram o =
  either renderError return =<< runExceptT (runReaderT (runApp run) o)

renderError :: AppError -> IO ()
renderError (IOError e) = do
  putStrLn "There was an error:"
  putStrLn $ "  " ++ show e

run :: App ()
run = do
  liftIO $ setLocaleEncoding utf8
  flip when (liftIO downloadSitemaps) =<< asks downloadsitemaps
  flip when (liftIO getBookUrls) =<< asks parsesitemapurls
  flip when (liftIO clearTables) =<< asks droptable
  flip when (liftIO createDTOs) =<< asks createdtos
  flip when (liftIO recreateTables) =<< asks recreatetables
  flip when (liftIO recreateAllTables) =<< asks recreatealltables
  flip when (liftIO scrapeMockBooks) =<< asks scrapemockbooks
  flip when (liftIO scrapeOneMockBook) =<< asks scrapeonemockbook
  flip when (liftIO createMockData) =<< asks mockdata
  flip when (liftIO scrapePopularList) =<< asks scrapepopularlist
  flip when (liftIO recursiveScrape) =<< asks scraperecursive
  -- scrapeId <- asks scrapebookid
  -- case scrapeId of
  -- Nothing -> pure ()
  -- Just id -> do
  -- _ <- liftIO $ scrapeBookId $ T.pack id
  -- pure ()
  flip when (liftIO server) =<< asks startserver

-- liftIO server

parseCLI :: IO Opts
parseCLI = execParser (withInfo parseOptions "nwdir")
  where
    withInfo opts h = info (helper <*> opts) $ header h

parseOptions :: Parser Opts
parseOptions =
  -- TODO: get rid of this layer, annoying to add switches in 4 places
  do
    downloadsitemaps <- switch $ long "downloadsitemaps"
    parsesitemapurls <- switch $ long "parsesitemapurls"
    scrapepopularlist <- switch $ long "scrapepopularlist"
    scraperecursive <- switch $ long "scraperecursive"
    startserver <- switch $ long "startserver"
    recreatetables <- switch $ long "recreatetables"
    recreatealltables <- switch $ long "recreatealltables"
    createnewtables <- switch $ long "createnewtables"
    scrapemockbooks <- switch $ long "scrapemockbooks"
    scrapeonemockbook <- switch $ long "scrapeonemockbook"
    mockdata <- switch $ long "mockdata"
    droptable <- switch $ long "droptable"
    migrate <- switch $ long "migrate"
    createdtos <- switch $ long "createdtos"
    scrapebookid <- optional $ strOption $ long "scrapebookid"
    return
      Opts
        { scrapepopularlist = scrapepopularlist,
          scraperecursive = scraperecursive,
          downloadsitemaps = downloadsitemaps,
          parsesitemapurls = parsesitemapurls,
          startserver = startserver,
          recreatetables = recreatetables,
          recreatealltables = recreatealltables,
          droptable = droptable,
          createnewtables = createnewtables,
          mockdata = mockdata,
          createdtos = createdtos,
          scrapebookid = scrapebookid,
          scrapemockbooks = scrapemockbooks,
          scrapeonemockbook = scrapeonemockbook
        }
