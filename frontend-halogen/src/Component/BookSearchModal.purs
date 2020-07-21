module Components.BookSearchModal where

import Styling
import BookTypeAhead as BookTypeAhead
import CSS as CSS
import Capability.AppCapabilities (class AppCapabilities)
import DTOs (Book)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Prelude (Unit, Void, bind, const, discard, pure, unit, ($), (*>), (<<<))
import Store (StoreAction, Store)
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (toEvent)

type ChildSlots
  = ( bookSearch :: BookTypeAhead.Slot Unit )

type Slot
  = H.Slot (Const Void) Message

data Action
  = None

feedCardBackground = light3

feedCardForeground = dark1

type InputRow
  = ( prompt :: String
    , cta :: String
    )

type Input
  = Record (InputRow)

data Message
  = CloseModal
  | SubmitRequest Book

component :: forall m. AppCapabilities StoreAction Store m => H.Component HH.HTML (Const Void) (Input) Message m
component =
  Hooks.component \{ outputToken } input -> Hooks.do
    selectedBook /\ selectedBookId <- Hooks.useState Nothing
    submitting /\ submittingId <- Hooks.useState false
    let
      submitBookRequest = do
        selectedBook <- Hooks.get selectedBookId
        case selectedBook of
          (Just book) -> do
            Hooks.modify_ submittingId (const true)
            Hooks.raise outputToken (SubmitRequest book)
            pure unit
          Nothing -> pure unit
        pure unit

      {-- closeModal = do --}
      {-- liftEffect $ log "Closing the modal!" --}
      {-- Hooks.raise outputToken CloseModal --}
      handleTypeAheadAction act = case act of
        BookTypeAhead.SelectionsChanged book -> do
          Hooks.modify_ selectedBookId (const (Just book))
          submitBookRequest
          pure unit
        _ -> pure unit
    Hooks.pure
      $ HH.div
          [ style
              $ fullHeight
              *> fullWidth
              *> CSS.zIndex 2
              *> modalBackground
              *> flexRow
              *> centerCenter
              *> fixed
              *> top (scaleToGeo S0)
              *> left (scaleToGeo S0)
          , HE.onClick \_ -> Just $ Hooks.raise outputToken (CloseModal)
          ]
          [ HH.div
              [ style $ modal
                  *> bgColor light2
                  *> padding S5
                  *> rounded0
                  *> fontColor dark1
              , HE.onClick \e ->
                  Just
                    $ do
                        liftEffect $ log "Preventing default!"
                        _ <- liftEffect $ stopPropagation $ toEvent e
                        pure unit
              ]
              [ HH.div [ style $ fontSize F2 *> fontColor dark3 *> weightBold ] [ HH.text input.prompt ]
              , spaceY S5
              , HH.slot (SProxy :: SProxy "bookSearch") unit BookTypeAhead.component unit (Just <<< handleTypeAheadAction)
              {-- , renderWhen (isJust selectedBook) $ HH.div [ HE.onClick \_ -> Just submitBookRequest, style $ padding S4 *> bgColor dark2 *> fontColor light2 *> fontSize F1 *> rounded0 ] [ HH.text input.cta ] --}
              ]
          ]
