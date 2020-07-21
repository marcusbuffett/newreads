module Component.Router where

import Prelude (Unit, Void, absurd, bind, const, discard, pure, show, unit, when, ($), (*>), (/=), (<$>), (<<<), (<>))
import Capability.AppCapabilities (class AppCapabilities)
import Capability.Navigate (navigate)
import Data.Const (Const)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Route (Route(..), routeCodec)
import Data.Symbol (SProxy(..))
import Effect.Class.Console (log)
import Halogen (ClassName(..), liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties (class_)
import Page.Home as Home
import Page.Request as Request
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Slug as Slug
import Store (StoreAction, Store)
import Styling (FontSize(..), bgColor, dark2, defaultLineHeight, fontBody, fontColor, fontSize, light1)
import Halogen.HTML.CSS (style)

type State
  = { route :: Maybe Route
    }

data Query a
  = Navigate Route a

data Action
  = Initialize
  | Receive {}

type ChildSlots
  = ( home :: OpaqueSlot Int
    , request :: OpaqueSlot Int
    , submitRequest :: OpaqueSlot Int
    )

type OpaqueSlot
  = H.Slot (Const Void) Void

component ::
  forall m.
  AppCapabilities StoreAction Store m =>
  H.Component HH.HTML Query {} Void m
component =
  H.mkComponent
    { initialState: const { route: Nothing }
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleQuery = handleQuery
              , handleAction = handleAction
              , receive = Just <<< Receive
              , initialize = Just Initialize
              }
    }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction = case _ of
    Initialize -> do
      initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getHash
      navigate $ fromMaybe Home initialRoute
      pure unit
    Receive {} -> do
      pure unit

  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
  handleQuery = case _ of
    Navigate dest a -> do
      { route } <- H.get
      liftEffect $ log $ "test, do we get in here?" <> show dest
      when (route /= Just dest) do
        liftEffect $ log "How about here?"
        H.modify_ _ { route = Just dest }
      pure (Just a)

  render :: State -> H.ComponentHTML Action ChildSlots m
  render { route } = do
    -- a reset of sorts
    HH.div [ class_ (ClassName "blah"), style $ defaultLineHeight *> fontBody *> fontColor light1 *> fontSize F0 *> bgColor dark2 ]
      [ case route of
          Just r -> case r of
            Home -> HH.slot (SProxy :: _ "home") 0 Home.component unit absurd
            Request s -> HH.slot (SProxy :: _ "request") 2 Request.component { requestUuid: Slug.toString s } absurd
          Nothing -> HH.div_ [ HH.text "Oh no! That page wasn't found." ]
      ]
