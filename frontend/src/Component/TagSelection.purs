module Component.TagSelection (Slot, Message(..), component, Tags, Tag) where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Styling as S

type Slot
  = H.Slot (Const Void) Message

data Action
  = TagToggled String

type State
  = { tags :: Tags }

type Tag
  = String

type Tags
  = Set Tag

data Message
  = TagsChanged Tags

allTags =
  [ "Story"
  , "Main character"
  , "Supporting characters"
  , "Narration"
  , "Worldbuilding"
  , "Pace"
  ]

component :: forall m. MonadAff m => H.Component HH.HTML (Const Void) Unit Message m
component =
  H.mkComponent
    { initialState: const { tags: Set.empty }
    , render: render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  handleAction :: Action -> H.HalogenM State Action () Message m Unit
  handleAction = case _ of
    TagToggled t -> do
      s <- H.get
      let
        newTags = if Set.member t s.tags then Set.delete t s.tags else Set.insert t s.tags
      H.modify_ _ { tags = newTags }
      H.raise $ TagsChanged newTags

render state =
  S.row S.S4 Nothing
    $ map createTag allTags
  where
  createTag tag =
    HH.div [ HE.onClick \_ -> Just (TagToggled tag) ]
      [ HH.p
          [ style
              $ do
                  if Set.member tag state.tags then S.bgColor S.dark1 else S.bgColor S.dark2
                  S.px S.S4
                  S.py S.S3
                  S.rounded0
          ]
          [ HH.text tag
          ]
      ]
