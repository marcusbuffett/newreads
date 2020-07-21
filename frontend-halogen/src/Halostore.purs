module Halostore where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Routing.Hash (matchesWith)
import Routing.Duplex (parse)
import Data.Route (routeCodec)
import Data.Maybe (Maybe(..))
import Control.Alt (class Functor)
import Control.Alternative (class Applicative, class Apply, apply)
import Control.Bind (class Bind)
import Control.Monad.Reader (ReaderT(..))
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (log)
import Halogen (HalogenM)
import Halogen as H
import Halogen.Component (ComponentSlot)
import Halogen.Component (mkComponent)
import Halogen.HTML as HH
import Halogen.Query.HalogenQ as HQ
import Halogen.Query.EventSource (EventSource(..), Emitter(..), affEventSource, emit)
import Halogen.Query.EventSource as ES
import Effect.Aff (error, forkAff, killFiber)
import Control.Monad.Rec.Class (forever)
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, new, read, write)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Coroutine as CC
import Data.Either (Either(..))
import Data.Bifunctor (class Bifunctor)
import Effect.Aff.AVar as AV
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)
import Data.Symbol (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Halogen.HTML.Events as HE
import Effect.Aff.Bus as Bus
import Effect.Aff.Bus (BusRW)
import Debug.Trace (trace)

busEventSource :: forall m r act. MonadAff m => Bus.BusR' r act -> EventSource m act
busEventSource bus =
  affEventSource \emitter -> do
    fiber <- forkAff $ forever $ emit emitter =<< Bus.read bus
    pure (ES.Finalizer (killFiber (error "Event source closed") fiber))

type Env action store
  = { value :: Ref store
    , bus :: BusRW store
    , update :: store -> action -> store
    }

class
  Monad m <= MonadStore a s m | m -> s a where
  updateStore :: a -> m Unit
  readStore :: m s
  readBus :: m (BusRW s)

newtype StoreM action store a
  = StoreM (ReaderT (Env action store) Aff a)

derive newtype instance functorStoreM :: Functor (StoreM action store)

derive newtype instance applyStoreM :: Apply (StoreM action store)

derive newtype instance applicativeStoreM :: Applicative (StoreM action store)

derive newtype instance bindStoreM :: Bind (StoreM action store)

derive newtype instance monadStoreM :: Monad (StoreM action store)

derive newtype instance monadEffectStoreM :: MonadEffect (StoreM action store)

derive newtype instance monadAffStoreM :: MonadAff (StoreM action store)

instance monadConnectStoreM :: MonadStore action store (StoreM action store) where
  readStore = StoreM $ liftEffect <<< read <<< _.value =<< ask
  readBus = StoreM $ pure <<< _.bus =<< ask
  updateStore action = StoreM $ (ask >>= \e -> inner e action)

inner :: forall action store. Env action store -> action -> ReaderT (Env action store) Aff Unit
inner env action =
  liftAff
    $ do
        store <- liftEffect $ read env.value
        let
          newStore = env.update store action
        liftEffect $ write newStore env.value
        Bus.write newStore env.bus

runStore ::
  forall action store h q i o.
  Bifunctor h =>
  store ->
  (store -> action -> store) ->
  H.Component h q i o (StoreM action store) ->
  Aff (H.Component h q i o Aff)
runStore initialStore update comp =
  liftAff do
    env <-
      liftEffect
        $ do
            bus <- liftEffect Bus.make
            value <- new initialStore
            pure { value, bus, update }
    pure $ H.hoist (flip runStoreM $ env) comp

runStoreM :: forall action store a. StoreM action store a -> Env action store -> Aff a
runStoreM (StoreM m) env = liftAff $ runReaderT m env

type WrapperState store input
  = { store :: Maybe store
    , input :: input
    }

data ConnectQuery s i o a
  = Update s a
  {-- | Proxy (Coyoneda f a) --}
  | Receive i a
  | Raise o a

data ConnectAction action
  = InitializeStore
  | HandleStoreAction action
  | HandleStoreBusUpdate

{-- | Update s a --}
{-- | Proxy (Coyoneda f a) --}
{-- | Receive i a --}
{-- | Raise o a --}
coerceConnect ::
  forall m a s f i o.
  MonadStore a s m =>
  H.Component HH.HTML (ConnectQuery s i o) Unit Void m ->
  H.Component HH.HTML (ConnectQuery s i o) Unit Void m
coerceConnect = unsafeCoerce

-- TODO: action -> update, a -> u
type ConnectedChildSlots store action
  = ( innerComponent :: H.Slot (CompQuery store) action Unit )

_innerComponent :: SProxy "innerComponent"
_innerComponent = SProxy

connect ::
  forall m a s f i o.
  MonadStore a s m =>
  MonadAff m =>
  H.Component HH.HTML (CompQuery s) (ConnectInput s i) a m ->
  H.Component HH.HTML (Const Void) i Void m
connect originalComponent =
  let
    render :: WrapperState s i -> H.ComponentHTML (ConnectAction a) (ConnectedChildSlots s a) m
    render = case _ of
      {-- { input, store: Just store } -> HH.slot unit (HC.mkComponent comp) { input, store } (HE.input Raise) --}
      { store: Just store, input } -> HH.div [] [ HH.slot _innerComponent unit originalComponent { store, input } (Just <<< HandleStoreAction) ]
      _ -> HH.text "blah"

    {-- eval :: ConnectQueryX s f i o ~> H.HalogenM (State s i) (ConnectQueryX s f i o) f Unit o m --}
    handleAction :: (ConnectAction a) -> H.HalogenM (WrapperState s i) (ConnectAction a) (ConnectedChildSlots s a) Void m Unit
    handleAction action = case action of
      InitializeStore -> do
        currentStore <- H.lift readStore
        storeBus <- H.lift readBus
        H.modify_ _ { store = Just currentStore }
        _ <- H.subscribe ((const HandleStoreBusUpdate) <$> busEventSource storeBus)
        pure unit
      HandleStoreBusUpdate -> do
        (currentStore :: s) <- H.lift readStore
        H.modify_ _ { store = Just currentStore }
        liftEffect $ (trace currentStore $ \_ -> log "SUbscribition store?")
        _ <- H.query _innerComponent unit $ H.tell $ StoreUpdated currentStore
        pure unit
      HandleStoreAction x -> do
        liftEffect $ log "Updating store?"
        currentStore <- H.lift readStore
        H.lift $ updateStore x
        {-- H.query _innerComponent unit $ H.tell $ StoreUpdated currentStore --}
        pure unit
  {-- H.subscribe $ connectedSource \store -> Update store --}
  {-- Proxy q -> --}
  {-- unCoyoneda --}
  {-- ( \k q' -> do --}
  {-- result <- H.query unit q' --}
  {-- case result of --}
  {-- Nothing -> HQ.halt "[Halogen.Component.Connect] Inner query failure" --}
  {-- Just r -> pure (k r) --}
  {-- ) --}
  {-- q --}
  {-- Update s next -> H.modify _ { store = Just s } $> next --}
  {-- Receive i next -> H.modify _ { input = i } $> next --}
  {-- Raise o next -> H.raise o $> next --}
  in
    H.mkComponent
      { initialState: { store: Nothing, input: _ }
      , render
      , eval:
          H.mkEval
            $ H.defaultEval
                { handleAction = handleAction
                -- receive = Just <<< Receive
                , initialize = Just InitializeStore
                }
      }

bogusHandleAction :: String -> String
bogusHandleAction x = ""

data CompQuery s a
  = StoreUpdated s a

type ConnectInput s i
  = { store :: s
    , input :: i
    }
