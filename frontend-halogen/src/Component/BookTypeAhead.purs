module BookTypeAhead where

import Prelude (Unit, Void, append, bind, const, discard, map, not, otherwise, pure, show, unit, void, (#), ($), (&&), (*>), (/=), (<#>), (<<<), (==), (>), (>>=), (>>>))
import Styling (FontSize(..), Scale(..), absolute, bgColor, border, clickable, column, dark0, dark2, dark3, fontColor, fontSize, fullWidth, left, light0, light2, light3, oneLineText, padding, relative, right, rounded0, scaleToGeo, shadow, spaceY, top, weightSemibold)
import CSS as CSS
import Capability.Api as Api
import Capability.AppCapabilities (class AppCapabilities)
import DTOs (Book)
import Data.Array (deleteAt)
import Data.Array (deleteAt, difference, filter, length, mapWithIndex, null, take, (!!))
import Data.Const (Const)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Maybe (fromJust)
import Data.Monoid (guard)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks (useEvent)
import Network.RemoteData as RD
import Partial.Unsafe (unsafePartial)
import Select (SelectEvent(..), SelectReturn(..), selectInput, useSelect)
import Select as Select
import Store (StoreAction, Store)
import Web.HTML.HTMLElement (focus)

type Slot
  = H.Slot (Const Void) Message

data Query a
  = GetSelections (Maybe Book -> a)

data Message
  = ItemRemoved Book
  | SelectionsChanged Book

type ChildSlots
  = ()

classes_ :: forall p i. Array String -> HH.IProp ( class :: String | i ) p
classes_ = HP.classes <<< map HH.ClassName

whenElem cond render = if cond then render unit else HH.text ""

whenJustElem (Just x) render = render x

whenJustElem Nothing render = HH.text ""

exampleBooks =
  [ "Refactoring UI by Adam Wathan"
  , "Skulduggery Pleasant by Derek Landy"
  , "Seveneves by Stephen Neal"
  , "Shoe Dog by Phil Knight"
  , "Skyward by Brandon Sanderson"
  , "The Giver by Lois Lowry"
  , "The Rage of Dragons by Evan Winter"
  , "The Name of The Wind by Patrick Rothfuss"
  , "Recursion by Blake Crouch"
  ]

component :: forall m. AppCapabilities StoreAction Store m => H.Component HH.HTML Query Unit Message m
component =
  Hooks.component \tokens _ -> Hooks.do
    let
      refLabel = H.RefLabel "inputElement"
    selections /\ selectionsId <- Hooks.useState Nothing
    available /\ availableId <- Hooks.useState RD.NotAsked
    selectEvents <- useEvent
    SelectReturn select <-
      useSelect
        $ selectInput
            { inputType = Select.Text
            , debounceTime = Just (Milliseconds 300.0)
            , getItemCount = pure $ maybe 0 length $ RD.toMaybe available
            , pushSelectedIdxChanged =
              \idx -> do
                selectEvents.push $ Select.VisibilityChangedTo Select.Off
                selectEvents.push $ SelectedIndex idx
            , pushNewSearch = selectEvents.push <<< NewSearch
            }
    Hooks.useLifecycleEffect do
      Hooks.getHTMLElementRef refLabel >>= traverse_ (focus >>> liftEffect)
      pure Nothing
    Hooks.useLifecycleEffect do
      {-- liftEffect $ focus inputElem --}
      void $ selectEvents.setCallback
        $ Just \_ val -> case val of
            Select.VisibilityChangedTo Select.Off -> do
              select.clearSearch
              select.setFocus false
            {-- Hooks.raise tokens.outputToken $ SelectionsChanged item --}
            SelectedIndex ix -> do
              available' <- Hooks.get availableId
              for_ available' \arr ->
                for_ (arr !! ix) \item -> do
                  Hooks.modify_ availableId $ const (RD.Success (filter (_ /= item) arr))
                  Hooks.put selectionsId (Just item)
                  select.clearSearch
                  Hooks.raise tokens.outputToken $ SelectionsChanged item
            NewSearch str -> do
              selection <- Hooks.get selectionsId
              Hooks.put availableId RD.Loading
              items <- liftAff $ searchLocations str
              Hooks.put availableId $ items <#> \xs -> difference xs (fromMaybe [] $ map (\x -> [ x ]) selection)
            _ -> pure unit
      pure Nothing
    Hooks.useQuery tokens.queryToken case _ of
      GetSelections reply -> do
        pure $ Just $ reply selections
    Hooks.pure
      $ HH.div
          [ HP.class_ (ClassName "Typeahead") ]
          [ maybe (renderInput select selections (renderContainer select selections available) refLabel) (renderSelection tokens.outputToken selectionsId) selections
          , spaceY S5
          ]
  where
  remove tOutput selectionsId item = do
    Hooks.put selectionsId Nothing
    Hooks.raise tOutput $ ItemRemoved item

  renderSelection tOutput selectionsId selection =
    HH.div
      [ HP.class_ (ClassName "Typeahead__selections") ]
      [ renderSelectedItem selection ]
    where
    renderSelectedItem item =
      HH.div
        [ HP.class_ (ClassName "Typeahead__item--selected Location") ]
        [ HH.span
            [ HP.class_ (ClassName "Location__name")
            , style $ bgColor light3 *> padding S3 *> fontColor dark0 *> rounded0
            ]
            [ HH.text item.title ]
        , closeButton item
        ]

    closeButton item =
      HH.span
        [ HP.class_ (ClassName "Location__closeButton")
        , HE.onClick \_ -> Just $ remove tOutput selectionsId item
        ]
        [ HH.text "×" ]

  renderInput select selection container ref =
    HH.div [ style $ relative ]
      [ HH.input
          ( (unsafePartial $ fromJust $ deleteAt 4 $ select.setInputProps [])
              `append`
                [ classes_
                    [ "Typeahead__input"
                    , "Typeahead__input--selections" # guard (isJust selection)
                    , "Typeahead__input--active"
                        # guard (select.visibility == Select.On)
                    ]
                , HP.placeholder ""
                , HP.ref ref
                , style $ fullWidth
                    *> fontSize F1
                    *> padding S4
                    *> border S3 light0
                    *> bgColor light0
                    *> fontColor dark2
                    *> rounded0
                {-- *> border (solid) (scaleToGeo S2) light0 --}
                -- TODO: style button
                -- TODO: position the dropdown correctly (below input)
                -- TODO: style dropdown list items, title and author
                ]
          )
      , container
      ]

  renderContainer select selection available =
    whenElem (select.visibility == Select.On && hasSearched) \_ ->
      HH.div
        [ style
            $ padding S4
            *> rounded0
            *> bgColor dark3
            *> absolute
            *> top (CSS.px 44.0)
            *> left (scaleToGeo S0)
            *> shadow
            *> right (scaleToGeo S0)
        ]
        renderItems
    where
    hasItems = maybe false (not <<< null) (RD.toMaybe available)

    hasSearched = case available of
      RD.NotAsked -> false
      _ -> true

    renderItems = do
      let
        renderMsg msg = [ HH.div [ style $ fontColor light3 *> fontSize F1 ] [ HH.text msg ] ]
      case available of
        RD.NotAsked -> [ HH.div [] [] ] -- Shouldn't be hit
        RD.Loading -> renderMsg "Loading..."
        RD.Failure e -> renderMsg $ show e
        RD.Success available'
          | length available' > 0 -> [ HH.div [ classes_ [ "items" ] ] [ column S6 (Nothing) $ renderItem `mapWithIndex` (take 5 available') ] ]
          | otherwise -> renderMsg "No results found"

    -- TODO: render HH.text "" if no search
    -- TODO: render no results in a better way
    -- TODO: don't set loading if there are already results? maybe just an
    -- additional flag? So we can show results while searching too
    -- TODO: Style button, more padding above
    renderItem index { title, author } =
      HH.div
        ( select.setItemProps index
            [ classes_ [ base, highlight, "Location" ] ]
        )
        [ column S2 (Just clickable)
            [ HH.div [ style $ fontSize F1 *> fontColor light2 *> weightSemibold *> oneLineText ] [ HH.text title ]
            , HH.div [ style $ fontSize F0 *> fontColor light3 *> oneLineText ] [ HH.text author ]
            ]
        ]
      where
      base = "Typeahead__item"

      highlight =
        "Typeahead__item--highlighted"
          # guard (select.highlightedIndex == Just index)

searchLocations "" = pure $ RD.Success []

searchLocations search = do
  Api.searchBooks search
