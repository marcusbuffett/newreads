module Utils.ConditionalRendering where

import Halogen.HTML as HH

renderWhen cond r = if cond then r else HH.text ""
