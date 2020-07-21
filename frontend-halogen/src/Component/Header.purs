module Components.Header where

import Prelude (Void, const, discard, unit, ($), (*>), (<>))
import Capability.AppCapabilities (class AppCapabilities)
import Component.InfoHeader as InfoHeader
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Route (Route(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Store (StoreAction, Store)
import Styling (FontSize(..), Scale(..), alignCenter, bgColor, column, container, dark1, flex, flexRow, fontColor, fontHeader, fontSize, fullWidth, justifyBetween, justifyCenter, light0, ls1, spaceX, spaceY, weightRegular)
import Utils.HTML (safeHref)
import Halogen.Hooks as Hooks
import Data.Monoid (guard)
import Icons as Icons
import Components.Icon (icon)

type Slot
  = H.Slot (Const Void) Message

data Message
  = ClickRequestRecs

data Action
  = ClickedGetRecs

type Input
  = { home :: Boolean
    }

component :: forall m. AppCapabilities StoreAction Store m => H.Component HH.HTML (Const Void) Input Message m
component =
  Hooks.component \{ outputToken } input -> do
    let
      handleAction :: Action -> _
      handleAction = case _ of
        ClickedGetRecs -> Hooks.raise outputToken ClickRequestRecs
    Hooks.pure
      $ column S0 (Just $ fullWidth)
      $ [ column S0
            ( Just
                $ do
                    bgColor light0
                    flex
                    justifyBetween
                    alignCenter
            )
            [ spaceY S7, renderLogo, spaceY S7
            ]
        ]
      <> guard input.home
          [ spaceY S6
          , HH.slot (SProxy :: _ "infoHeader") unit InfoHeader.component unit (const Nothing)
          , spaceY S6
          ]

renderLogo :: ∀ p a. HH.HTML p a
renderLogo =
  HH.a [ safeHref Home, style $ flexRow *> alignCenter *> container *> justifyCenter ]
    [ icon S7 dark1 Icons.iconBookClose
    , spaceX S5
    , HH.h1
        [ style $ fontSize F1 *> weightRegular *> ls1 *> fontHeader *> fontColor dark1
        ]
        [ HH.text "NEW READS" ]
    ]
