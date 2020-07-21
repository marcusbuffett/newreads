module Components.RequestCard where

import Prelude (Unit, Void, pure, unit, ($), (*>), (<>))
import Capability.AppCapabilities (class AppCapabilities)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Store (StoreAction, Store)
import Styling as S
import Record as Record
import Halogen.HTML.Properties (classes)
import DTOs as DTO

type ChildSlots
  = ()

type Slot
  = H.Slot (Const Void) Void

data Action
  = None

type InputRow
  = ( request :: DTO.Request )

type Input
  = Record InputRow

type State
  = Record ( | InputRow )

feedCardBackground = S.light3

feedCardForeground = S.dark1

component :: forall m. AppCapabilities StoreAction Store m => H.Component HH.HTML (Const Void) Input Void m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState :: Input -> State
  initialState i = Record.merge i {}

  handleAction :: forall m. Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction a = case a of
    None -> pure unit

  render :: State -> _
  render state =
    HH.div
      [ style $ S.bgColor feedCardBackground *> S.padding S.S6 *> S.rounded0 *> S.shadow
      , classes [ HH.ClassName "feedRequestCard" ]
      ]
      [ HH.div [ style $ S.flexRow ]
          [ HH.div
              []
              []
          , HH.div [ style $ S.flexColumn *> S.fontColor feedCardForeground *> S.fullWidth ]
              [ HH.a
                  [ HP.href $ "/#/request/" <> request.uuid ]
                  [ HH.p
                      [ style $ S.fontSize S.F1 *> S.opaqueHigh ]
                      -- TODO: follow pattern from:
                      -- https://github.com/purescript-halogen/purescript-halogen/issues/324
                      -- , source code at bottom
                      [ HH.text $ "Recommendation requested for:" ]
                  , S.column S.S0 (Nothing)
                      [ HH.h2 []
                          [ HH.b
                              [ style $ S.fontHeader *> S.fontSize S.F2 *> S.weightSemibold
                              , classes [ HH.ClassName "bookTitle" ]
                              ]
                              [ HH.text request.book.title ]
                          ]
                      , S.row S.S0 Nothing
                          [ HH.b [ style $ S.fontSize S.F1 *> S.weightRegular *> S.mr S.S4 ] [ HH.text " by " ]
                          , HH.b
                              [ style $ S.fontHeader *> S.fontSize S.F1 *> S.weightSemibold *> S.ls1
                              ]
                              [ HH.text $ request.book.author ]
                          ]
                      ]
                  ]
              ]
          ]
      ]
    where
    request = state.request
