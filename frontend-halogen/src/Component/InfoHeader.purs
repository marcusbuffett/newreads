module Component.InfoHeader (Slot, Message(..), component) where

import Prelude
import Capability.Persist (getPersist, updatePersist)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Styling as S
import Capability.AppCapabilities (class AppCapabilities)
import Store (StoreAction, Store)
import Halogen.HTML.Properties as HP

type Slot
  = H.Slot (Const Void) Message

data Action
  = CloseClicked
  | Initialize

type State
  = { closed :: Boolean
    }

data Message
  = Clicked

-- Q: String? Maybe Void?
component :: forall m. AppCapabilities StoreAction Store m => H.Component HH.HTML (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: const { closed: false }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }
  where
  handleAction :: Action -> H.HalogenM State Action () Void m Unit
  handleAction = case _ of
    CloseClicked -> do
      {-- _ <- H.modify \st -> st { closeClicked = true } --}
      _ <- H.lift $ updatePersist \p -> p { infoHeaderClosed = true }
      H.modify_ \st -> st { closed = true }
      pure unit
    Initialize -> do
      persist <- H.lift $ getPersist
      H.modify_ \st -> st { closed = persist.infoHeaderClosed }

render s = case s.closed of
  true -> HH.div_ []
  false ->
    HH.div
      [ style
          $ S.flex
          *> S.centerCenter
          *> S.fullWidth
      ]
      [ S.column S.S6 (Just $ S.container *> S.selfCenter)
          [ HH.p
              [ style $ S.fontSize S.F0 *> S.fontColor S.light2 *> S.bodyLineHeight ]
              [ HH.text "Exchange recommendations with fellow book lovers. For now, all revenue from affiliate links goes to ", HH.a [ style $ S.clickable *> S.weightSemibold, HP.href "https://www.givedirectly.org/" ] [ HH.text " GiveDirectly" ], HH.text ", which sends cash to people living in poverty." ]
          {-- , HH.div --}
          {-- [ HE.onClick $ const (Just CloseClicked) --}
          {-- , style --}
          {-- $ S.selfEnd --}
          {-- *> S.bgColor S.dark0 --}
          {-- *> S.fontColor S.light1 --}
          {-- *> S.fontSize S.F2 --}
          {-- *> S.weightSemibold --}
          {-- *> S.py S.S4 --}
          {-- *> S.px S.S6 --}
          {-- *> S.rounded0 --}
          {-- ] --}
          {-- [ HH.text "Cool!" ] --}
          ]
      ]
