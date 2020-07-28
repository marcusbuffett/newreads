module Utils.HTML where

import Prelude
import Data.Route (Route, routeCodec)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)

safeHref :: forall r i. Route -> HH.IProp ( href :: String | r ) i
safeHref = HP.href <<< append "#" <<< print routeCodec
