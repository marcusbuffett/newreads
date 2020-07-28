module Capability.Persist where

import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)
import Prelude
import Data.Route as Route
import Control.Monad.Trans.Class (lift)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen (HalogenM, liftAff)
import Routing.Hash (setHash)
import Routing.Duplex (print)
import Data.Route (routeCodec)
import Simple.JSON (readJSON, writeJSON)
import Simple.JSON as JSON
import Data.Either (Either(..), hush)
import Effect.Class.Console (log)
import Data.Maybe (Maybe(..))
import Capability.AppM (AppM)

type PersistState
  = { infoHeaderClosed :: Boolean
    }

key = "persisted_data" :: String

defaultState :: PersistState
defaultState = { infoHeaderClosed: false }

getPersistentState :: Aff PersistState
getPersistentState = do
  str <- liftEffect $ getItem key =<< localStorage =<< window
  case str of
    Nothing -> pure $ defaultState
    Just str -> case readJSON str of
      Right (state :: PersistState) -> do
        pure $ state
      {-- log $ show r --}
      Left e -> do
        liftEffect $ log $ show e
        pure $ defaultState

setPersistentState :: PersistState -> Aff Unit
setPersistentState state =
  let
    jsonStr = writeJSON state
  in
    do
      liftEffect $ (setItem key jsonStr) =<< localStorage =<< window

-- read json
-- | This capability represents the ability to move around the application. The `navigate` function 
-- | should change the browser location, which will then notify our routing component. The `logout`
-- | function should clear any information associated with the user from the app and browser before
-- | redirecting them to the homepage.
class
  Monad m <= Persist m where
  updatePersist :: (PersistState -> PersistState) -> m Unit
  getPersist :: m PersistState

{-- instance persistAff :: Persist Aff where --}
{-- updatePersist f = do --}
{-- s <- getPersistentState --}
{-- setPersistentState $ f s --}
{-- getPersist = do --}
{-- getPersistentState --}
instance persistAppM :: Persist (AppM action store) where
  updatePersist f = do
    s <- liftAff $ getPersistentState
    liftAff $ setPersistentState $ f s
  getPersist = do
    liftAff $ getPersistentState
