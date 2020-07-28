module Main where

{-- import Prelude --}
import Component.Router as Router

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Halogen.Aff as HA

import Halogen.VDom.Driver (runUI)
import Routing.Hash (matchesWith)
import Routing.Duplex (parse)
import Data.Route (routeCodec)
import Data.Maybe (Maybe(..))
import Capability.AppM (runApp)
import Prelude (Unit, bind, discard, pure, unit, void, when, ($), (/=))







import Effect.Class.Console (log)

import Halogen as H








{-- import FRP (FRP) --}





















import Store (initialStore, reduce)

main :: Effect Unit
main =
  HA.runHalogenAff do
    rootComponent <- runApp initialStore reduce Router.component
    {-- rootComponent :: H.Component HH.HTML Query {} Void Aff --}
    {-- rootComponent = H.hoist (runAppM env) Router.component --}
    body <- HA.awaitBody
    halogenIO <-
      runUI rootComponent {} body
    void $ liftEffect
      $ matchesWith (parse routeCodec) \old new ->
          when (old /= Just new) do
            liftEffect $ log "changed!"
            launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new
    pure unit
