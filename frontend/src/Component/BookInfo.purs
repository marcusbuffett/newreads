module Components.BookInfo where

import Prelude (Void, ($))
import Capability.AppCapabilities (class AppCapabilities)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Store (StoreAction, Store)
import Styling as S
import Halogen.HTML.Properties (classes)
import DTOs as DTO
import Halogen.Hooks as Hooks

type ChildSlots
  = ()

type Slot
  = H.Slot (Const Void) Void

data Action
  = None

type InputRow
  = ( book :: DTO.Book )

type Input
  = Record InputRow

component :: forall m. AppCapabilities StoreAction Store m => H.Component HH.HTML (Const Void) Input Void m
component =
  Hooks.component \_ input -> Hooks.do
    Hooks.pure
      $ HH.div []
          [ HH.div
              [ style $ S.cardBookTitleStyling
              , classes [ HH.ClassName "bookTitle" ]
              ]
              [ HH.text input.book.title ]
          , S.spaceY S.S2
          , S.row S.S0 Nothing
              [ HH.div [ style $ S.cardBookBylineStyling ] [ HH.text " by " ]
              , HH.div
                  [ style $ S.cardBookAuthorStyling
                  ]
                  [ HH.text $ input.book.author ]
              ]
          ]
