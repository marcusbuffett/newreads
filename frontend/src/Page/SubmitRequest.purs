module Page.SubmitRequest (component) where

import Prelude
import Affjax (get, printError)
import Affjax.ResponseFormat (string)
import BookTypeAhead as BookTypeAhead
import CSS as CSS
import Data.Array (replicate)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Route (Route(..))
import Data.Symbol (SProxy(..))
import Data.Unfoldable (none)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FetchButton as FetchButton
import Foreign (MultipleErrors, renderForeignError)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Simple.JSON (readJSON)
import Styling as S
import Utils.HTML (safeHref)
import DTOs (Book)

data Action
  = HandleFetch FetchButton.Message
  | CheckButtonState

type State
  = { books :: Array Book
    , width :: Int
    }

type ChildSlots
  = ( fetchButton :: FetchButton.Slot Unit
    , bookSearch :: BookTypeAhead.Slot Unit
    )

_button :: SProxy "button"
_button = SProxy

component :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ =
  { width: 1200
  , books: none
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div
    [ style
        $ S.bgColor S.dark1
        *> CSS.minHeight (CSS.vh 100.0)
        *> S.fontColor S.light0
        *> S.fontSize S.F2
    ]
    ( [ renderNav state
      ]
        <> map (\x -> HH.div [] [ HH.text $ x.title <> "blah" ]) state.books
    )

renderNav :: ∀ a. State → HH.HTML a Action
renderNav state =
  HH.div
    [ style
        $ do
            S.px S.S6
            S.py S.S6
            S.bgColor S.dark0
            S.fontColor S.light0
            S.flex
            S.justifyBetween
    ]
    [ renderLogo state
    ]

renderRecentRecSection :: ∀ a. State → HH.HTML a Action
renderRecentRecSection state =
  HH.div
    [ style
        $ S.flexColumn
        *> S.py S.S5
        *> CSS.maxWidth (CSS.px 800.0)
        *> S.mxAuto
    {-- *> S.alignCenter --}
    {-- *> S.bgColor S.dark2 --}
    ]
    [ HH.h1
        [ style $ CSS.maxWidth (CSS.px 800.0)
            *> S.py S.S3
            *> S.fontColor S.light0
            *> S.fontSize S.F2
            *> S.weightSemibold
        ]
        [ HH.text "Recent recommendations" ]
    , HH.div
        [ style $ S.flexColumn
        ]
        ( replicate 5 $ renderRecCard state
        )
    ]

renderRecCard :: ∀ a. State → HH.HTML a Action
renderRecCard state =
  let
    s = S.S4
  in
    HH.div
      [ style $ S.bgColor S.dark2 *> S.py s *> S.px s *> S.mb S.S6 *> S.rounded0 *> S.shadow
      ]
      [ HH.div [ style $ S.flexRow ]
          [ HH.div
              [ style $ S.bgColor S.dark2 ]
              []
          , HH.div [ style $ S.flexColumn *> S.fontColor S.light0 ]
              [ HH.h2
                  [ style $ S.fontHeader *> S.fontSize S.F2 *> S.pb S.S3 ]
                  [ HH.text "The Name of the Wind" ]
              , HH.p
                  [ style $ S.fontSize S.F1 ]
                  [ HH.text "A heroic fantasy novel written by American author Patrick Rothfuss. It is the first book in the ongoing fantasy trilogy The Kingkiller Chronicle, followed by The Wise Man's Fear." ]
              ]
          ]
      ]

renderLogo state =
  HH.a [ safeHref Home ]
    [ HH.h1
        [ style $ S.fontSize S.F3 *> S.weightBold *> S.ls1 *> S.fontHeader
        ]
        [ HH.text "NWREADS " ]
    ]

{-- renderCheckPart :: ∀ a. State → HH.HTML a Action --}
{-- renderCheckPart state = --}
{-- HH.p_ --}
{-- [ HH.text --}
{-- $ "Last time I checked, the button was: " --}
{-- <> (maybe "(not checked yet)" (if _ then "on" else "off") state.buttonState) --}
{-- <> ". " --}
{-- , HH.button --}
{-- [ HE.onClick (\_ -> Just CheckButtonState) ] --}
{-- [ HH.text "Check now" ] --}
{-- ] --}
handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  HandleFetch FetchButton.Clicked -> do
    liftEffect $ log "blah, parent"
    res <- liftAff $ get string "http://localhost:4200/book/Harry"
    case lmap transformAjaxError res >>= (lmap transformJsonError <<< readJSON <<< _.body) of
      Right (r :: Array Book) -> do
        liftEffect $ log "all good"
        liftEffect $ log $ show r
        H.modify_ (\x -> x { books = r })
      {-- log $ show r --}
      Left e -> do
        liftEffect
          $ do
              log "all bad"
              log "test"
              log $ show e
  CheckButtonState -> do
    liftEffect $ log "x"

transformAjaxError err = singleton $ StringError $ printError err

transformJsonError :: MultipleErrors -> NonEmptyList StringError
transformJsonError err = map (StringError <<< renderForeignError) err

data StringError
  = StringError String

instance showStringError :: Show StringError where
  show (StringError str) = str
