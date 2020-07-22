module Styling where

import Prelude (compose, discard, ($), (*>), (<), (<<<))
import CSS (solid, textWhitespace, toHSLA)
import CSS as CSS
import CSS.Background (backgroundColor)
import CSS.Border as Border
import CSS.Common as CSSCommon
import CSS.Display as Display
import CSS.Flexbox as Flexbox
import CSS.Font as Font
import CSS.Geometry as Geo
import CSS.Overflow as Overflow
import CSS.Size as Size
import CSS.String (fromString)
import CSS.String as CSSString
import CSS.Stylesheet (key)
import CSS.Text as CSSText
import CSS.Text as Text
import CSS.TextAlign as TextAlign
import Color (hsl, hsla)
import Data.Array (uncons, (:))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Web.HTML (window)
import Web.HTML.Window (innerWidth)

data Scale
  = S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11
  | S12

scaleValues =
  [ S0
  , S1
  , S2
  , S3
  , S4
  , S5
  , S6
  , S7
  , S8
  , S9
  , S10
  , S11
  , S12
  ]

scaleToGeo :: Scale -> Size.Size Size.Abs
scaleToGeo s = case s of
  S0 -> Size.px 0.0
  S1 -> Size.px 1.0
  S2 -> Size.px 2.0
  S3 -> Size.px 4.0
  S4 -> Size.px 8.0
  S5 -> Size.px 12.0
  S6 -> Size.px 16.0
  S7 -> Size.px 24.0
  S8 -> Size.px 32.0
  S9 -> Size.px 48.0
  S10 -> Size.px 64.0
  S11 -> Size.px 128.0
  S12 -> Size.px 256.0

infixl 9 compose as ∘

data FontSize
  = F0
  | F1
  | F2
  | F3
  | F4

fontSizeValues =
  [ F0
  , F1
  , F2
  , F3
  , F4
  ]

fontSizeToPx :: FontSize -> Size.Size Size.Abs
fontSizeToPx fs = case fs of
  F0 -> Size.px 14.0
  F1 -> Size.px 18.0
  F2 -> Size.px 24.0
  F3 -> Size.px 32.0
  F4 -> Size.px 48.0

mx :: Scale -> CSS.CSS
mx s = do
  Geo.marginLeft $ scaleToGeo s
  Geo.marginRight $ scaleToGeo s

my :: Scale -> CSS.CSS
my s =
  let
    p = scaleToGeo s
  in
    do
      Geo.marginTop p
      Geo.marginBottom p

mt :: Scale -> CSS.CSS
mt s = do
  Geo.marginTop $ scaleToGeo s

mr :: Scale -> CSS.CSS
mr s = do
  Geo.marginRight $ scaleToGeo s

mb :: Scale -> CSS.CSS
mb s = do
  Geo.marginBottom $ scaleToGeo s

ml :: Scale -> CSS.CSS
ml s = do
  Geo.marginLeft $ scaleToGeo s

padding :: Scale -> CSS.CSS
padding s = Geo.padding (scaleToGeo s) (scaleToGeo s) (scaleToGeo s) (scaleToGeo s)

margin :: Scale -> CSS.CSS
margin s = Geo.margin (scaleToGeo s) (scaleToGeo s) (scaleToGeo s) (scaleToGeo s)

px :: Scale -> CSS.CSS
px s =
  Geo.paddingLeft (scaleToGeo s)
    *> Geo.paddingRight (scaleToGeo s)

py :: Scale -> CSS.CSS
py s =
  let
    p = scaleToGeo s
  in
    Geo.paddingTop p *> Geo.paddingBottom p

pt :: Scale -> CSS.CSS
pt s = do
  Geo.paddingTop $ scaleToGeo s

pr :: Scale -> CSS.CSS
pr s = do
  Geo.paddingRight $ scaleToGeo s

pb :: Scale -> CSS.CSS
pb s = do
  Geo.paddingBottom $ scaleToGeo s

pl :: Scale -> CSS.CSS
pl s = do
  Geo.paddingLeft $ scaleToGeo s

fontSize :: FontSize -> CSS.CSS
fontSize s =
  let
    p = fontSizeToPx s
  in
    Font.fontSize p

flex :: CSS.CSS
flex = Display.display Display.flex

flexColumn :: CSS.CSS
flexColumn = Display.display Display.flex *> Flexbox.flexDirection Flexbox.column

flexRow :: CSS.CSS
flexRow = Display.display Display.flex *> Flexbox.flexDirection Flexbox.row

justifyAround :: CSS.CSS
justifyAround = Flexbox.justifyContent Flexbox.spaceAround

justifyBetween :: CSS.CSS
justifyBetween = Flexbox.justifyContent Flexbox.spaceBetween

justifyCenter :: CSS.CSS
justifyCenter = Flexbox.justifyContent CSSCommon.center

justifyStart :: CSS.CSS
justifyStart = Flexbox.justifyContent Flexbox.flexStart

justifyEnd :: CSS.CSS
justifyEnd = Flexbox.justifyContent Flexbox.flexEnd

alignCenter :: CSS.CSS
alignCenter = Flexbox.alignItems CSSCommon.center

alignStart :: CSS.CSS
alignStart = Flexbox.alignItems Flexbox.flexStart

alignEnd :: CSS.CSS
alignEnd = Flexbox.alignItems Flexbox.flexEnd

selfEnd :: CSS.CSS
selfEnd = Flexbox.alignSelf Flexbox.flexEnd

selfCenter :: CSS.CSS
selfCenter = Flexbox.alignSelf CSSCommon.center

selfStart :: CSS.CSS
selfStart = Flexbox.alignSelf Flexbox.flexStart

centerCenter :: CSS.CSS
centerCenter = justifyCenter *> alignCenter

textRight :: CSS.CSS
textRight = TextAlign.textAlign TextAlign.rightTextAlign

textCenter :: CSS.CSS
textCenter = TextAlign.textAlign TextAlign.center

textLeft :: CSS.CSS
textLeft = TextAlign.textAlign TextAlign.leftTextAlign

greysHue = 202.0

dark0 = hsl greysHue 0.05 0.03

dark1 = hsl greysHue 0.05 0.06

dark2 = hsl greysHue 0.05 0.10

dark3 = hsl greysHue 0.05 0.15

dark4 = hsl greysHue 0.05 0.20

dark5 = hsl greysHue 0.05 0.25

dark6 = hsl greysHue 0.05 0.40

light0 = hsl greysHue 0.05 0.97

light1 = hsl greysHue 0.05 0.93

light2 = hsl greysHue 0.05 0.85

light3 = hsl greysHue 0.05 0.76

light4 = hsl greysHue 0.05 0.60

transparent = hsla greysHue 0.0 0.0 0.0

modalBackground =
  let
    c =
      (toHSLA dark0)
        { a = 0.5
        }
  in
    bgColor $ hsla c.h c.s c.l c.a

primaryHue = 202.0

primary0 = hsl primaryHue 0.90 0.85

onPrimaryColor = hsl primaryHue 0.90 0.15

bgColor = backgroundColor

fontColor = Font.color

weightLight = Font.fontWeight $ Font.weight 300.0

weightRegular = Font.fontWeight $ Font.weight 400.0

weightSemibold = Font.fontWeight $ Font.weight 600.0

weightBold = Font.fontWeight $ Font.weight 700.0

height = CSS.height

width = CSS.width

fullWidth = CSS.width $ CSS.pct 100.0

fullHeight = CSS.height $ CSS.pct 100.0

ls0 = Text.letterSpacing $ Size.rem 0.0

ls1 = Text.letterSpacing $ Size.rem 0.07

fontHeader = Font.fontFamily [ "Lato" ] (singleton Font.sansSerif)

fontBody = Font.fontFamily [ "Lato" ] (singleton Font.sansSerif)

rounded0 =
  let
    r = Size.px 2.0
  in
    Border.borderRadius r r r r

rounded1 =
  let
    r = Size.px 8.0
  in
    Border.borderRadius r r r r

rounded2 =
  let
    r = Size.px 16.0
  in
    Border.borderRadius r r r r

roundedFull =
  let
    r = Size.px 999.0
  in
    Border.borderRadius r r r r

mxAuto = CSS.marginLeft CSSCommon.auto *> CSS.marginRight CSSCommon.auto

shadow =
  let
    c =
      (toHSLA dark0)
        { a = 0.2
        }

    color = hsla c.h c.s c.l c.a
  in
    CSS.boxShadow (Size.nil) (Size.px 4.0) (Size.px 10.0) color

row :: Scale -> Maybe CSS.CSS -> _ -> _
row spacing styling children = HH.div [ style $ flexRow ?> styling ] (innerChildren spacing)
  where
  innerChildren S0 = children

  innerChildren s = interleave (spaceX s) children

column :: Scale -> Maybe CSS.CSS -> Array (HH.HTML _ _) -> _
column spacing styling children = HH.div [ style $ flexColumn ?> styling ] (innerChildren spacing)
  where
  innerChildren S0 = children

  innerChildren s = interleave (spaceY s) children

{-- spacer styling = HH.div [ style $ styling] --}
interleave :: forall a. a -> Array a -> Array a
interleave s cs = case uncons cs of
  Nothing -> []
  Just { head: c, tail: [] } -> [ c ]
  Just { head: c, tail: cs } -> c : s : interleave s cs

opacity = key $ CSSString.fromString "opacity"

opaqueFull = opacity 1.0

opaqueHigh = opacity 0.9

opaqueMid = opacity 0.7

opaqueLow = opacity 0.5

breakpointMobile = 540

spaceY s = HH.div [ style $ pt s ] []

spaceX s = HH.div [ style $ pr s ] []

{-- breakpointMobile = Size.px 540.0 --}
whenMobile :: forall a. a -> Maybe a
whenMobile s = if (unsafePerformEffect (innerWidth $ unsafePerformEffect window)) < breakpointMobile then Just s else Nothing

{-- onMobile :: CSS.CSS -> CSS.CSS --}
{-- onMobile styles = Stylesheet.query Media.screen (singleton $ Media.maxWidth (breakpointMobile)) styles --}
{-- query :: MediaType -> NonEmpty Array Feature -> CSS -> CSS --}
maybeApplySecond :: CSS.CSS -> Maybe CSS.CSS -> CSS.CSS
maybeApplySecond a (Just b) = a *> b

maybeApplySecond a (Nothing) = a

infixl 4 maybeApplySecond as ?>

container = CSS.maxWidth (CSS.px 800.0) *> px S5 *> fullWidth

modal = fullWidth *> mx S7 *> CSS.maxWidth (CSS.px 500.0)

constrain = CSS.maxWidth (CSS.pct 100.0) *> CSS.maxHeight (CSS.pct 100.0)

constrainX = CSS.maxWidth (CSS.pct 100.0)

maxWidth = CSS.maxWidth <<< scaleToGeo

maxHeight = CSS.maxHeight <<< scaleToGeo

fixed = CSS.position CSS.fixed

absolute = CSS.position CSS.absolute

static = CSS.position CSS.static

relative = CSS.position CSS.relative

clickable = CSS.key (fromString "cursor") "pointer"

top = Geo.top

right = Geo.right

left = Geo.left

bottom = Geo.bottom

oneLineText = (textWhitespace $ CSS.whitespaceNoWrap) *> Overflow.overflow Overflow.hidden *> CSS.key (fromString "text-overflow") "ellipsis"

defaultLineHeight = CSS.key (fromString "line-height") "1.3"

bodyLineHeight = CSS.key (fromString "line-height") "1.5"

underline = CSSText.textDecoration CSSText.underline

flexGrow = Flexbox.flexGrow 1

border s c = Border.border solid (scaleToGeo s) c

basis0 = CSS.key (fromString "flex-basis") "0.0"

css k v = CSS.key (fromString k) v

-- class-esque stuff
listHeaderStyling = fontSize F0 *> weightSemibold *> ls1 *> fontColor light3

tabStyling = fontSize F1 *> weightRegular *> ls0 *> fontColor light2 *> clickable

cardHeaderStyling = fontSize F0 *> fontColor light3

cardBookTitleStyling = fontHeader *> fontSize F1 *> weightSemibold *> fontColor light0

cardBookBylineStyling = fontSize F0 *> weightRegular *> mr S3 *> fontColor light2

cardBookAuthorStyling = fontSize F0 *> weightSemibold *> fontColor light3
