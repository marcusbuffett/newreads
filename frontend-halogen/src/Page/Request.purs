module Page.Request (component) where

import Prelude (Unit, Void, bind, const, discard, map, pure, unit, ($), (*>), (<<<), (<>), (==), (>))
import Utils.ConditionalRendering (renderWhen)
import BookTypeAhead as BookTypeAhead
import CSS as CSS
import Capability.Api as Api
import Capability.AppCapabilities (class AppCapabilities)
import Components.BookSearchModal as BookSearchModal
import Components.Header as Header
import Components.Icon (icon, iconCustom)
import Components.RecommendationCard as RecommendationCard
import DTOs (Book)
import DTOs as DTO
import Data.Array (length)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Effect.Aff.Class (liftAff)
import FetchButton as FetchButton
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Halostore (CompQuery, ConnectInput, connect)
import Icons as Icons
import Network.RemoteData as RD
import Store (Store, StoreAction)
import Styling (FontSize(..), Scale(..), alignCenter, bgColor, centerCenter, clickable, column, constrainX, container, dark5, flexColumn, flexGrow, flexRow, fontColor, fontSize, fullWidth, justifyBetween, light1, light2, light3, listHeaderStyling, maxWidth, mr, padding, rounded0, row, spaceY, transparent, weightBold, weightRegular, weightSemibold)

data Action
  = FetchRequest

{-- | SetRecModalOpen Boolean --}
type State
  = { books :: Array Book
    , width :: Int
    , requestUuid :: String
    , request :: Maybe DTO.Request
    , recommendations :: Maybe (Array DTO.Recommendation)
    , searchModalOpen :: Boolean
    }

type ChildSlots
  = ( fetchButton :: FetchButton.Slot Unit
    , bookSearch :: BookTypeAhead.Slot Unit
    , requestForm :: (H.Slot (Const Void) Void) Unit
    , header :: Header.Slot Unit
    , rec :: RecommendationCard.Slot String
    , recModal :: BookSearchModal.Slot Unit
    )

type Input
  = Record
      ( requestUuid :: String
      )

_button :: SProxy "button"
_button = SProxy

component :: forall m. AppCapabilities StoreAction Store m => H.Component HH.HTML (Const Void) Input Void m
component = connect innerComponent

innerComponent :: forall m. AppCapabilities StoreAction Store m => H.Component HH.HTML (CompQuery Store) (ConnectInput Store Input) StoreAction m
innerComponent =
  Hooks.component \_ input -> Hooks.do
    state /\ stateId <- Hooks.useState $ initialState input
    store /\ storeId <- Hooks.useState $ input.store
    recModalOpen /\ recModalOpenId <- Hooks.useState $ false
    Hooks.useLifecycleEffect do
      r <- liftAff $ Api.fetchRequest state.requestUuid
      case r of
        RD.Success { request, recommendations } -> do
          {-- log $ show request --}
          {-- pure unit --}
          Hooks.modify_ stateId (_ { request = Just request, recommendations = Just recommendations })
          pure Nothing
        _ -> pure Nothing
    let
      handleModalMessage message = case message of
        BookSearchModal.CloseModal -> Hooks.modify_ recModalOpenId (const false)
        BookSearchModal.SubmitRequest book -> do
          resp <- liftAff $ Api.submitRecommendation state.requestUuid book.uuid
          case resp of
            RD.Success recommendation -> do
              case state.recommendations of
                (Just recommendations) -> do
                  Hooks.modify_ stateId (_ { recommendations = Just $ [ recommendation ] <> recommendations })
                Nothing -> pure unit
            _ -> pure unit
          Hooks.modify_ recModalOpenId (const false)

      handleHeaderAction :: Header.Message -> Hooks.HookM m Unit
      handleHeaderAction action = case action of
        Header.ClickRequestRecs -> Hooks.modify_ recModalOpenId (const true)

      renderBody = case state of
        { recommendations: Just recommendations, request: Just request } -> do
          HH.div [ style $ flexColumn *> flexGrow *> container ]
            ( [ spaceY S7
              , HH.div
                  [ style
                      $ fontSize F3
                      *> fontColor light2
                      *> weightSemibold
                  ]
                  [ HH.text request.book.title ]
              , row S0 Nothing
                  [ HH.div
                      [ style $ fontSize F0 *> weightRegular *> mr S3 *> fontColor light3 ]
                      [ HH.text " by " ]
                  , HH.div
                      [ style $ fontSize F0 *> weightSemibold *> fontColor light3 ]
                      [ HH.text $ request.book.author ]
                  ]
              , spaceY S9
              , HH.div
                  [ style $ listHeaderStyling
                  ]
                  [ HH.text "RECOMMENDATIONS" ]
              , spaceY S5
              , renderWhen (length recommendations > 0) $ column S5 Nothing $ renderAddRecCard <> map createRecCard recommendations
              , renderWhen (length recommendations == 0) $ column S5 (Just $ flexGrow) $ renderAddRecCard <> [ spaceY S5, renderEmpty ]
              ]
                <> guard recModalOpen
                    [ HH.slot (SProxy :: SProxy "recModal") unit BookSearchModal.component
                        { prompt: prompt request.book, cta }
                        (Just <<< handleModalMessage)
                    ]
            )
        _ ->
          HH.div [ style $ flexGrow *> centerCenter *> flexColumn ]
            [ HH.div [ style $ maxWidth S10 ] [ HH.img [ style $ constrainX, HP.src "public/loaders/three-dots.svg" ] ]
            ]

      renderAddRecCard =
        [ HH.div
            [ style
                $ fullWidth
                *> padding S6
                *> fontSize F1
                *> flexRow
                *> justifyBetween
                *> bgColor dark5
                *> fontColor light1
                *> weightSemibold
                *> clickable
                *> rounded0
                *> alignCenter
            , HE.onClick \_ -> Just $ Hooks.modify_ recModalOpenId (const true)
            ]
            [ HH.text "Add recommendation"
            , HH.div [ style $ rounded0 *> bgColor light1 *> padding S5 ] [ iconCustom [ HP.classes $ [ HH.ClassName "iconAdd" ] ] S6 transparent Icons.iconAddBold ]
            ]
        ]

      renderEmpty =
        HH.div
          [ style $ flexColumn
              *> flexGrow
              *> centerCenter
              *> fontSize F1
              *> fontColor light3
          ]
          [ icon S9 light3 Icons.iconSmileySad1
          , HH.div [ style $ padding S7 ]
              [ HH.text "There aren't any recommendations yet. You can check back later, or "
              , HH.a
                  [ HE.onClick \_ -> Just $ Hooks.modify_ recModalOpenId (const true)
                  , style $ maxWidth S8 *> weightBold *> fontColor light1 *> clickable
                  ]
                  [ HH.text "add one" ]
              ]
          ]
    Hooks.pure $ HH.div [ style $ CSS.minHeight (CSS.vh 100.0) *> flexColumn *> centerCenter ] [ HH.slot (SProxy :: _ "header") unit Header.component { home: false } (\a -> Just $ handleHeaderAction a), renderBody ]
  where
  -- TODO: map with an index, to avoid halogen error here
  createRecCard :: AppCapabilities StoreAction Store m => DTO.Recommendation -> H.ComponentHTML _ ChildSlots m
  createRecCard rec = HH.slot (SProxy :: _ "rec") rec.uuid RecommendationCard.component { recommendation: rec, showHeader: false, includeMeta: false, link: Just rec.book.amazonLink } (const Nothing)

  prompt reqBook = "Recommend a book to readers of " <> reqBook.title

  cta = "Recommend Book"

initialState :: { store :: Store, input :: Input } -> State
initialState { store, input } =
  { width: 1200
  , books: none
  , requestUuid: input.requestUuid
  , request: Nothing
  , recommendations: Nothing
  , searchModalOpen: false
  }
