module Components.RecommendationMeta where

import Icons (iconHyperlink3)
import Prelude (Unit, Void, pure, unit, ($), (*>))
import CSS (Color)
import DTOs as DTOs
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Record as Record
import Styling as S
import Components.Icon (icon)

type ChildSlots
  = ()

type Slot
  = H.Slot (Const Void) Void

data Action
  = None

type InputRow
  = ( color :: Color, recommendation :: DTOs.Recommendation )

type Input
  = Record InputRow

type State
  = Record ( hoverState :: Boolean | InputRow )

component :: forall m. MonadAff m => H.Component HH.HTML (Const Void) Input Void m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: Input -> State
  initialState i = Record.merge i { hoverState: false }

  handleAction :: forall m. Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction a = case a of
    None -> pure unit

  render :: State -> _
  render state =
    S.row S.S0 (Just $ S.alignCenter)
      {-- [ icon S.S7 state.color (iconEcologyLeaf []) --}
      {-- , S.spaceX S.S4 --}
      {-- , HH.div [ style $ S.fontSize S.F2 *> S.weightLight ] [ HH.text $ NumberFormat.toString state.recommendation.leaves ] --}
      {-- , S.spaceX S.S7 --}
      [ HH.a [ HP.href state.recommendation.book.amazonLink, style $ S.py S.S4 *> S.px S.S5 *> S.bgColor S.dark2 *> S.fontColor S.light1 *> S.rounded0 ] [ icon S.S7 state.color (iconHyperlink3) ]
      ]
