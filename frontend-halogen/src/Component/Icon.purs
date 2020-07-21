module Components.Icon where

import Prelude
import CSS (Color)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Styling

icon :: Scale -> Color -> _ -> _
-- *> CSkey (CSKey (CSPlain "strokeColor")) "red" 
icon s c i = iconCustom [] s c i

iconCustom :: _ -> Scale -> Color -> _ -> _
-- *> CSkey (CSKey (CSPlain "strokeColor")) "red" 
iconCustom props s c i = HH.div ([ style $ width size *> height size *> fontColor c *> flexColumn *> centerCenter ] <> props) [ i [ style $ fullWidth *> fullHeight ] ]
  where
  size = (scaleToGeo s)
