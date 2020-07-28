module Component.HalostoreDemo where

import Prelude
import Component.Router as Router
import Component.Router (Query)
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
import Capability.AppM (runAppM)
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
import Halostore (class MonadStore, CompQuery(..), connect)
import Store (StoreAction(..), Store)

data ComponentAction
  = SetUsername String

type CompState
  = { store :: Store
    }

component ::
  forall r m.
  MonadAff m =>
  MonadStore StoreAction Store m =>
  H.Component HH.HTML (Const Void) Store Void m
component =
  connect
    $ H.mkComponent
        { initialState: \s -> { store: s.store }
        , render
        , eval:
          H.mkEval
            $ H.defaultEval
                { handleAction = handleAction
                , handleQuery = handleQuery
                }
        -- handleQuery = handleQuery-- handleAction = handleAction 
        }
  where
  handleQuery :: forall a. CompQuery Store a -> H.HalogenM CompState ComponentAction () StoreAction m (Maybe a)
  handleQuery query = case query of
    StoreUpdated s next -> do
      liftEffect $ log "blah"
      H.put { store: s }
      pure $ Just next

  handleAction :: ComponentAction -> H.HalogenM CompState ComponentAction () StoreAction m Unit
  handleAction action = case action of
    SetUsername x -> do
      st <- H.get
      H.raise $ UpdateUserName (st.store.userName <> "+")

  render :: CompState -> H.ComponentHTML ComponentAction () m
  render state =
    HH.div [ HE.onClick \_ -> Just $ SetUsername "blah" ]
      [ HH.text (show state)
      ]
 {-- handleQuery query = case query of --} {-- ReceiveStore { store } next -> H.put store $> next --} {-- Store action next -> H.lift (updateStore action) $> next --}
