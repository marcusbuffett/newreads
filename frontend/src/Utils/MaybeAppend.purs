module Utils.MaybeAppend where

import Prelude
import Data.Semigroup (class Semigroup)
import Data.Semigroup
import Data.Maybe

maybeAppend :: forall a. Semigroup a => a -> Maybe (a) -> a
maybeAppend a (Just b) = a <> b

maybeAppend a Nothing = a

infixl 4 maybeAppend as <?>
