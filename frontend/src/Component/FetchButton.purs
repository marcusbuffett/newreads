module FetchButton (Slot, Message(..), component) where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Slot
  = H.Slot (Const Void) Message

data Action
  = ButtonClicked

type State
  = Unit

data Message
  = Clicked

-- Q: String? Maybe Void?
component :: forall m. MonadAff m => H.Component HH.HTML (Const Void) Unit Message m
component =
  H.mkComponent
    { initialState: const unit
    , render: const render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  handleAction :: Action -> H.HalogenM Unit Action () Message m Unit
  handleAction = case _ of
    ButtonClicked -> do
      liftEffect $ log "Blah"
      H.raise $ Clicked

render =
  HH.button
    [ HE.onClick $ const (Just ButtonClicked)
    ]
    [ HH.text "test" ]
