module Components.RecommendationCard where

import Capability.AppCapabilities (class AppCapabilities)
import Components.BookInfo as BookInfo
import Components.RecommendationMeta as RecommendationMeta
import DTOs as DTO
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Properties as HP
import Prelude (Unit, Void, const, pure, unit, ($), (*>), (<>))
import Record as Record
import Store (StoreAction, Store)
import Styling

type ChildSlots
  = ( meta :: RecommendationMeta.Slot Unit
    , bookInfo :: BookInfo.Slot Unit
    )

type Slot
  = H.Slot (Const Void) Void

data Action
  = HoverSaplings
  | ClickStore

type InputRow
  = ( recommendation :: DTO.Recommendation, showHeader :: Boolean, includeMeta :: Boolean, link :: Maybe String )

type Input
  = Record InputRow

type State
  = Record ( | InputRow )

feedCardBackground = light0

feedCardForeground = dark1

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

  handleAction :: Action -> H.HalogenM State Action ChildSlots Void m Unit
  handleAction a = case a of
    HoverSaplings -> pure unit
    ClickStore -> pure unit

  render :: State -> _
  render state =
    HH.div
      [ style $ bgColor feedCardBackground *> padding S6 *> rounded0 *> shadow *> fontColor feedCardForeground
      ]
      [ HH.a
          [ HP.href $ fromMaybe ("/#/request/" <> state.recommendation.request.uuid) (state.link) ]
          [ row S0 (Just $ alignCenter *> justifyBetween)
              [ column S0 (Nothing)
                  $ ( if state.showHeader then
                        [ HH.p
                            [ style $ cardHeaderStyling ]
                            [ HH.text $ "Recommended to readers of ", HH.span [ style $ weightBold ] [ HH.text state.recommendation.request.book.title ] ]
                        , spaceY S4
                        ]
                      else
                        []
                    )
                  <> [ HH.slot (SProxy :: _ "bookInfo") unit BookInfo.component { book: state.recommendation.book } (const Nothing) ]
                  <> ( if state.includeMeta then
                        [ HH.div
                            [ style $ selfEnd *> textRight ]
                            [ HH.slot (SProxy :: _ "meta") unit RecommendationMeta.component { color: light1, recommendation: state.recommendation } (const Nothing)
                            ]
                        ]
                      else
                        []
                    )
                  <> [ spaceY S4 ]
              {-- , HH.div [ style $ padding S5 *> opacity 0.4 ] [ iconCustom [] S8 dark0 Icons.iconECommerceAmazon ] --}
              ]
          ]
      ]
