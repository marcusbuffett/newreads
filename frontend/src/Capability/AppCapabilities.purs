module Capability.AppCapabilities where

import Capability.Persist (class Persist)
import Capability.Navigate (class Navigate)
import Effect.Aff.Class (class MonadAff)
import Capability.AppM (AppM)
import Halostore (class MonadStore)

class (Persist m, Navigate m, MonadAff m, MonadStore action store m) <= AppCapabilities action store m

{-- type AppMon --}
{-- = AppCapabilities Store StoreAction --}
instance appCapabilitiesAppM :: AppCapabilities action store (AppM action store)
