module Capability.AppM where

import Prelude
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halostore (Env, StoreM(..), runStoreM, class MonadStore, readStore, readBus, updateStore)
import Store (Store, StoreAction)
import Halogen as H
import Data.Bifunctor (class Bifunctor)
import Effect.Aff.Bus as Bus
import Effect.Ref (Ref, new, read, write)
import Undefined (undefined)

newtype AppM action store a
  = AppM (StoreM action store a)

newtype InnerM action store a
  = InnerM (ReaderT (Env action store) Aff a)

newtype OuterM action store a
  = OuterM (InnerM action store a)

{-- = AppM (ReaderT (Env StoreAction Store) Aff a) --}
derive newtype instance functorAppM :: Functor (AppM action store)

derive newtype instance applyAppM :: Apply (AppM action store)

derive newtype instance applicativeAppM :: Applicative (AppM action store)

derive newtype instance bindAppM :: Bind (AppM action store)

derive newtype instance monadAppM :: Monad (AppM action store)

derive newtype instance monadEffectAppM :: MonadEffect (AppM action store)

derive newtype instance monadAffAppM :: MonadAff (AppM action store)

runAppM :: forall a. Env StoreAction Store -> AppM StoreAction Store a -> Aff a
runAppM env (AppM storeM) = runStoreM storeM env

instance monadConnectAppM :: MonadStore action store (AppM action store) where
  readStore = AppM readStore
  readBus = AppM readBus
  updateStore action = AppM $ updateStore action

runApp ::
  forall h q i o.
  Bifunctor h =>
  Store ->
  (Store -> StoreAction -> Store) ->
  H.Component h q i o (AppM StoreAction Store) ->
  Aff (H.Component h q i o Aff)
runApp initialStore update comp =
  liftAff do
    env <-
      liftEffect
        $ do
            bus <- liftEffect Bus.make
            value <- new initialStore
            pure { value, bus, update }
    pure $ H.hoist (runAppM env) comp
