module Page.Home (component) where

import Prelude (class Eq, class Show, Ordering, Unit, Void, bind, compare, const, discard, map, pure, show, unit, ($), (*>), (+), (<$>), (<<<), (<>), (==))
import Button as Button
import CSS as CSS
import Capability.Api (fetchFeed, fetchFeedRequests, fetchFeedRecommendations)
import Capability.Api (submitRequest)
import Capability.AppCapabilities (class AppCapabilities)
import Capability.Navigate (navigate)
import Components.BookInfo as BookInfo
import Components.BookSearchModal as BookSearchModal
import Components.Header as Header
import Components.RecommendationCard as RecommendationCard
import Components.RecommendationMeta as RecommendationMeta
import DTOs as DTO
import Data.Array (catMaybes, concat, filter, find, sortBy)
import Data.Const (Const)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.Monoid (guard)
import Data.Route (Route(..))
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes)
import Halogen.HTML.Properties as HP
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import Halostore (CompQuery(..), ConnectInput, connect)
import Network.RemoteData as RD
import Partial.Unsafe (unsafePartial)
import Slug as Slug
import Store (Store, StoreAction)
import Styling (FontSize(..), Scale(..), alignCenter, bgColor, cardHeaderStyling, centerCenter, clickable, column, constrainX, container, dark1, dark5, flexColumn, flexGrow, flexRow, fontColor, fontSize, fullWidth, justifyBetween, light0, light1, light3, listHeaderStyling, ls1, maxWidth, mxAuto, padding, px, py, rounded0, row, shadow, spaceY, tabStyling, transparent, weightSemibold)
import Components.Icon (icon, iconCustom)
import Icons as Icons

data Action
  = HandleButton Button.Message
  | FetchRequests
  | AppendToEmail
  | ClickedRequestButton
  | HeaderAction

type State
  = { toggleCount :: Int
    , buttonState :: Maybe Boolean
    , store :: Store
    , feed :: Maybe (Array FeedItem)
    }

type ChildSlots
  = ( header :: Header.Slot Unit
    , meta :: RecommendationMeta.Slot Unit
    , rec :: RecommendationCard.Slot String
    , recModal :: BookSearchModal.Slot Unit
    , bookInfo :: BookInfo.Slot String
    )

data Tab
  = All
  | Requests
  | Recommendations

derive instance eqTab :: Eq Tab

component :: forall m. AppCapabilities StoreAction Store m => H.Component HH.HTML (Const Void) Unit Void m
component = connect innerComponent

innerComponent :: forall m. AppCapabilities StoreAction Store m => H.Component HH.HTML (CompQuery Store) (ConnectInput Store Unit) StoreAction m
innerComponent =
  Hooks.component \_ input -> Hooks.do
    -- TODO: hook for AppCapabilities store stuff
    state /\ stateId <- Hooks.useState $ initialState input
    offset /\ offsetId <- Hooks.useState $ 0
    loading /\ loadingId <- Hooks.useState $ false
    hasMore /\ hasMoreId <- Hooks.useState $ true
    currentTab /\ currentTabId <- Hooks.useState $ Recommendations
    recModalOpen /\ recModalOpenId <- Hooks.useState $ false
    let
      feedLimit = 20

      getFeedCurrentTab = case currentTab of
        All -> fetchFeed feedLimit
        Requests -> fetchFeedRequests feedLimit
        Recommendations -> fetchFeedRecommendations feedLimit
    Hooks.captures { currentTab } Hooks.useTickEffect do
      Hooks.modify_ stateId (_ { feed = Nothing })
      Hooks.modify_ offsetId (const 0)
      Hooks.modify_ loadingId (const true)
      r <-
        liftAff $ getFeedCurrentTab 0
      Hooks.modify_ loadingId (const false)
      case r of
        RD.Success f -> do
          Hooks.modify_ hasMoreId (const (f.offset == 0 + feedLimit))
          Hooks.modify_ stateId (_ { feed = Just (createMixedFeed f) })
          Hooks.modify_ offsetId (const f.offset)
          pure Nothing
        _ -> pure Nothing
    Hooks.captures { recModalOpen } Hooks.useTickEffect do
      liftEffect $ log $ show recModalOpen
      pure Nothing -- ... if no cleanup is required before the next run of the effect
    let
      handleHeaderAction :: Header.Message -> HookM m Unit
      handleHeaderAction action = case action of
        Header.ClickRequestRecs -> Hooks.modify_ recModalOpenId (const true)

      feedItemUuid (FeedItemRequest x) = x.uuid

      feedItemUuid (FeedItemRecommendation x) = x.uuid

      addToFeed stateFeed newFeed = stateFeed <> filter (\feedItem -> isNothing $ find (\x -> feedItemUuid feedItem == feedItemUuid x) stateFeed) newFeed

      nextPage :: HookM m Unit
      nextPage = do
        Hooks.modify_ loadingId (const true)
        r <- liftAff (getFeedCurrentTab offset)
        Hooks.modify_ loadingId (const false)
        case r of
          RD.Success f -> do
            Hooks.modify_ hasMoreId (const (f.offset == offset + feedLimit))
            Hooks.modify_ stateId (_ { feed = Just (addToFeed (fromMaybe [] state.feed) (createMixedFeed f)) })
            Hooks.modify_ offsetId (const f.offset)
            pure unit
          _ -> pure unit

      handleModalMessage message = case message of
        BookSearchModal.CloseModal -> Hooks.modify_ recModalOpenId (const false)
        BookSearchModal.SubmitRequest book -> do
          r <- liftAff $ submitRequest book.uuid
          case r of
            RD.Success f -> do
              _ <- H.lift $ navigate $ Request (unsafePartial $ fromJust $ Slug.parse f._requestUuid)
              pure unit
            _ -> pure unit
          Hooks.modify_ recModalOpenId (const false)

      activeTabStyling = style $ tabStyling *> fontColor light0 *> weightSemibold *> fontSize F1

      inactiveTabStyling = style $ tabStyling *> fontColor light3 *> fontSize F1

      renderAddRequestCard =
        HH.div
          [ style
              $ fullWidth
              *> px S6
              *> py S7
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
          [ HH.text "Request recommendations"
          , HH.div [ style $ rounded0 *> bgColor light1 *> padding S5 ] [ iconCustom [ HP.classes $ [ HH.ClassName "iconAdd" ] ] S6 transparent Icons.iconAddBold ]
          ]

      renderTab tab =
        HH.div
          [ if tab == currentTab then activeTabStyling else inactiveTabStyling
          , HE.onClick \_ -> Just $ Hooks.modify_ currentTabId (const tab)
          ]
          [ HH.text
              $ case tab of
                  All -> "All"
                  Requests -> "Requests"
                  Recommendations -> "Recommendations"
          ]

      pageNextCard =
        HH.div [ style $ fullWidth *> flexColumn *> centerCenter ]
          [ HH.div
              [ HE.onClick \_ -> Just $ nextPage
              , style $ bgColor dark1 *> fontColor light3 *> fontSize F0 *> shadow *> padding S6 *> rounded0 *> weightSemibold *> clickable *> ls1
              ]
              [ HH.text "LOAD MORE" ]
          ]

      renderFeedHeader =
        row S5 (Just $ listHeaderStyling *> flexRow)
          $ map renderTab [ Recommendations, Requests ]

      loader =
        HH.div [ style $ flexColumn *> alignCenter ]
          [ spaceY S10
          , HH.div [ style $ maxWidth S10 ] [ HH.img [ style $ constrainX, HP.src "public/loaders/three-dots.svg" ] ]
          ]

      renderEmpty =
        HH.div
          [ style $ flexColumn
              *> flexGrow
              *> centerCenter
              *> fontSize F0
              *> fontColor light3
          ]
          [ spaceY S5
          , icon S8 light3 Icons.iconSmileySad1
          , HH.div [ style $ padding S5 ]
              [ HH.text "You've reached the end! Check back later."
              ]
          ]

      renderRecentRecSection :: H.ComponentHTML _ ChildSlots m
      renderRecentRecSection =
        HH.div
          [ style
              $ flexColumn
              *> container
              *> py S5
              *> mxAuto
              *> flexGrow
          ]
          [ renderFeedHeader
          , spaceY S5
          , column S6 (Nothing) $ concat
              $ catMaybes
                  [ Just [ renderAddRequestCard ]
                  , (map feedCard <$> state.feed)
                  , if loading then Just [ loader ] else (if hasMore then Just [ pageNextCard ] else Just [ renderEmpty ])
                  ]
          ]
    Hooks.pure
      $ HH.div
          [ style
              $ CSS.minHeight (CSS.vh 100.0)
              *> fontColor light0
              *> fontSize F2
              *> flexColumn
          ]
          ( [ HH.slot (SProxy :: _ "header") unit Header.component { home: true } (\a -> Just $ handleHeaderAction a)
            , spaceY S5
            , renderRecentRecSection
            ]
              <> guard recModalOpen
                  [ HH.slot (SProxy :: SProxy "recModal") unit BookSearchModal.component { prompt, cta } (Just <<< handleModalMessage)
                  ]
          )
  where
  prompt = "Search for a book, and others will recommend you books like it."

  cta = "Get recommendations"

initialState :: ConnectInput Store Unit -> State
initialState { store, input } =
  { toggleCount: 0
  , store: store
  , buttonState: Nothing
  , feed: Nothing
  }

handleQuery :: forall a m. CompQuery Store a -> H.HalogenM State Action ChildSlots StoreAction m (Maybe a)
handleQuery query = case query of
  StoreUpdated s next -> do
    H.modify_ _ { store = s }
    pure $ Just next

feedCardBackground = light1

feedCardForeground = dark1

feedCard :: ∀ a. AppCapabilities StoreAction Store a => FeedItem -> H.ComponentHTML _
 ChildSlots a
feedCard (FeedItemRequest request) =
  HH.div
    [ style $ bgColor feedCardBackground *> padding S6 *> rounded0 *> shadow
    , classes [ HH.ClassName "feedRequestCard" ]
    ]
    [ HH.div [ style $ flexRow ]
        [ HH.div
            []
            []
        , HH.div [ style $ flexColumn *> fontColor feedCardForeground *> fullWidth ]
            [ HH.a
                [ HP.href $ "/#/request/" <> request.uuid ]
                [ HH.p
                    [ style $ cardHeaderStyling ]
                    [ HH.text $ "Recommendation requested for" ]
                , spaceY S3
                , HH.slot (SProxy :: _ "bookInfo") request.uuid BookInfo.component { book: request.book } (const Nothing)
                , spaceY S4
                ]
            ]
        ]
    ]

feedCard (FeedItemRecommendation recommendation) =
  HH.slot (SProxy :: _ "rec") recommendation.uuid RecommendationCard.component
    { recommendation: recommendation
    , showHeader: true
    , includeMeta: false
    , link: Nothing
    }
    (const Nothing)

maybeAppend :: ∀ a. Array a -> Maybe (Array a) -> Array a
maybeAppend xs (Just ys) = xs <> ys

maybeAppend xs Nothing = xs

data FeedItem
  = FeedItemRequest DTO.Request
  | FeedItemRecommendation DTO.Recommendation

derive instance genericFeedItem :: Generic FeedItem _

instance showFeedItem :: Show FeedItem where
  show = genericShow

feedItemFromRequest :: DTO.Request -> FeedItem
feedItemFromRequest request = FeedItemRequest request

feedItemFromRecommendation :: DTO.Recommendation -> FeedItem
feedItemFromRecommendation rec = FeedItemRecommendation rec

createMixedFeed :: DTO.FeedResponse -> Array FeedItem
createMixedFeed r =
  sortBy feedComparator $ map feedItemFromRequest (r.requests)
    <> map feedItemFromRecommendation (r.recommendations)

feedComparator :: FeedItem -> FeedItem -> Ordering
feedComparator a b = compare (creationDateFeedItem a) (creationDateFeedItem b)

creationDateFeedItem (FeedItemRecommendation x) = x.created

creationDateFeedItem (FeedItemRequest x) = x.created
