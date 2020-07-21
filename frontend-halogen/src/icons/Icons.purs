module Icons where

import Prelude ((<>))
import Halogen.HTML

ns :: Namespace
ns = Namespace "http://www.w3.org/2000/svg"


iconAddBold :: forall p r i. Array (IProp r i) -> HTML p i
iconAddBold attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "viewBox") "0 0 24 24"
  ])
  [ elementNS ns (ElemName "defs")
    [ ]
    [ elementNS ns (ElemName "style")
      [ ]
      [ text """.a{fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;}"""
      ]
    ], elementNS ns (ElemName "title")
    [ ]
    [ text """add-bold"""
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M23.5,9.5a1,1,0,0,0-1-1h-7v-7a1,1,0,0,0-1-1h-5a1,1,0,0,0-1,1v7h-7a1,1,0,0,0-1,1v5a1,1,0,0,0,1,1h7v7a1,1,0,0,0,1,1h5a1,1,0,0,0,1-1v-7h7a1,1,0,0,0,1-1Z"
    ]
    [ 
    ]
  ]

iconBookClose :: forall p r i. Array (IProp r i) -> HTML p i
iconBookClose attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "viewBox") "0 0 24 24"
  ])
  [ elementNS ns (ElemName "defs")
    [ ]
    [ elementNS ns (ElemName "style")
      [ ]
      [ text """.a{fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;}"""
      ]
    ], elementNS ns (ElemName "title")
    [ ]
    [ text """book-close"""
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M12,7.512l10-5V17.394a1,1,0,0,1-.553.894L12,23.012,2.553,18.288A1,1,0,0,1,2,17.394V2.512Z"
    ]
    [ 
    ], elementNS ns (ElemName "polyline")
    [ attr (AttrName "class") "a"
    , attr (AttrName "points") "19.999 0.988 11.999 5.036 3.999 0.988"
    ]
    [ 
    ], elementNS ns (ElemName "line")
    [ attr (AttrName "class") "a"
    , attr (AttrName "x1") "12"
    , attr (AttrName "y1") "7.512"
    , attr (AttrName "x2") "12"
    , attr (AttrName "y2") "23.012"
    ]
    [ 
    ]
  ]

iconBookLibrary1 :: forall p r i. Array (IProp r i) -> HTML p i
iconBookLibrary1 attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "viewBox") "0 0 24 24"
  ])
  [ elementNS ns (ElemName "defs")
    [ ]
    [ elementNS ns (ElemName "style")
      [ ]
      [ text """.a{fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;}"""
      ]
    ], elementNS ns (ElemName "title")
    [ ]
    [ text """book-library-1"""
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M23.5,22.5a1,1,0,0,1-1,1H1.5a1,1,0,0,1-1-1V1.5a1,1,0,0,1,1-1h21a1,1,0,0,1,1,1Z"
    ]
    [ 
    ], elementNS ns (ElemName "line")
    [ attr (AttrName "class") "a"
    , attr (AttrName "x1") "0.5"
    , attr (AttrName "y1") "12.5"
    , attr (AttrName "x2") "23.5"
    , attr (AttrName "y2") "12.5"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M7.5,23.5h-4V15a.5.5,0,0,1,.5-.5H7a.5.5,0,0,1,.5.5Z"
    ]
    [ 
    ], elementNS ns (ElemName "rect")
    [ attr (AttrName "class") "a"
    , attr (AttrName "x") "13.5"
    , attr (AttrName "y") "4.5"
    , attr (AttrName "width") "3"
    , attr (AttrName "height") "8"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M19.284,12.5,16.162,3.406a.5.5,0,0,1,.311-.635l1.892-.65A.5.5,0,0,1,19,2.432L22.456,12.5"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M13.5,12.5h-4V4a.5.5,0,0,1,.5-.5h3a.5.5,0,0,1,.5.5Z"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M11.5,23.5h-4V15a.5.5,0,0,1,.5-.5h3a.5.5,0,0,1,.5.5Z"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M20.5,23.5h-3V15a.5.5,0,0,1,.5-.5h2a.5.5,0,0,1,.5.5Z"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M17.5,23.5h-3V16a.5.5,0,0,1,.5-.5h2a.5.5,0,0,1,.5.5Z"
    ]
    [ 
    ], elementNS ns (ElemName "line")
    [ attr (AttrName "class") "a"
    , attr (AttrName "x1") "5.5"
    , attr (AttrName "y1") "17.5"
    , attr (AttrName "x2") "5.5"
    , attr (AttrName "y2") "20.5"
    ]
    [ 
    ], elementNS ns (ElemName "line")
    [ attr (AttrName "class") "a"
    , attr (AttrName "x1") "9.5"
    , attr (AttrName "y1") "17.5"
    , attr (AttrName "x2") "9.5"
    , attr (AttrName "y2") "20.5"
    ]
    [ 
    ]
  ]

iconECommerceAmazon1 :: forall p r i. Array (IProp r i) -> HTML p i
iconECommerceAmazon1 attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "viewBox") "0 0 24 24"
  ])
  [ elementNS ns (ElemName "defs")
    [ ]
    [ elementNS ns (ElemName "style")
      [ ]
      [ text """.a{fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;fill-rule:evenodd;}"""
      ]
    ], elementNS ns (ElemName "title")
    [ ]
    [ text """e-commerce-amazon-1"""
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M22.95,19.533c-12.3,6.719-21.594-.082-21.594-.082"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M20.593,23.337c1.432-.755,2.658-2.937,2.363-3.787l-.006-.017c-.26-.559-2.586-1.414-4.39-.7"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M12.928,7.9C8.383,8.24,4.3,8.995,4.3,13.686a4.146,4.146,0,0,0,4.369,4.52,5.751,5.751,0,0,0,4.823-2.133A7.944,7.944,0,0,0,15.163,18a.712.712,0,0,0,.71-.051c.61-.508,1.677-1.423,2.235-1.929a.551.551,0,0,0,.051-.761,3.8,3.8,0,0,1-1.118-2.692V8c0-1.929.153-3.708-1.269-5.025A6.82,6.82,0,0,0,11.354,1.5C8.1,1.5,5.4,2.829,4.8,5.969a.477.477,0,0,0,.407.609l2.843.354a.643.643,0,0,0,.508-.557A2.333,2.333,0,0,1,10.948,4.6a2,2,0,0,1,1.625.762A4.591,4.591,0,0,1,12.928,7.9Zm-.558,6.042c-.97,1.935-3.809,1.941-3.809-.762a2.67,2.67,0,0,1,2.184-2.742,9.984,9.984,0,0,1,2.183-.2v.61A5.391,5.391,0,0,1,12.37,13.941Z"
    ]
    [ 
    ]
  ]

iconECommerceAmazon :: forall p r i. Array (IProp r i) -> HTML p i
iconECommerceAmazon attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "viewBox") "0 0 24 24"
  ])
  [ elementNS ns (ElemName "defs")
    [ ]
    [ elementNS ns (ElemName "style")
      [ ]
      [ text """.a{fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;}"""
      ]
    ], elementNS ns (ElemName "title")
    [ ]
    [ text """e-commerce-amazon"""
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M6.225,4.8a4.875,4.875,0,0,1,4.484-3.3,4.955,4.955,0,0,1,4.769,4.99v6.564c0,1.148.66,2.415,1.713,2.415"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M15.478,7.66c-15.843-2.072-7.337,15.461,0,3.757"
    ]
    [ 
    ], elementNS ns (ElemName "polyline")
    [ attr (AttrName "class") "a"
    , attr (AttrName "points") "20.686 22.473 23 18.621 18.641 17.417"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M23,18.621A22.4,22.4,0,0,1,11.978,21.5,22.4,22.4,0,0,1,.941,18.612"
    ]
    [ 
    ]
  ]

iconEcologyLeaf :: forall p r i. Array (IProp r i) -> HTML p i
iconEcologyLeaf attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "id") "Light"
  , attr (AttrName "viewBox") "0 0 24 24"
  ])
  [ elementNS ns (ElemName "defs")
    [ ]
    [ elementNS ns (ElemName "style")
      [ ]
      [ text """.cls-1{fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;}"""
      ]
    ], elementNS ns (ElemName "title")
    [ ]
    [ text """ecology-leaf"""
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "cls-1"
    , attr (AttrName "d") "M15.768,19.554a11.581,11.581,0,0,1-5.5,1.5A7.5,7.5,0,0,1,5.819,7.516s2.449-2.462,8.449-2.462a13.871,13.871,0,0,0,7.89-2.021.5.5,0,0,1,.774.329C23.4,6.072,24.172,14.932,15.768,19.554Z"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "cls-1"
    , attr (AttrName "d") "M13.268,11.554A24.9,24.9,0,0,0,4.7,15.822L.81,19.138"
    ]
    [ 
    ]
  ]

iconHyperlink3 :: forall p r i. Array (IProp r i) -> HTML p i
iconHyperlink3 attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "viewBox") "0 0 24 24"
  ])
  [ elementNS ns (ElemName "defs")
    [ ]
    [ elementNS ns (ElemName "style")
      [ ]
      [ text """.a{fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;}"""
      ]
    ], elementNS ns (ElemName "title")
    [ ]
    [ text """hyperlink-3"""
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M12.47,18.105,8.243,22.332a3.987,3.987,0,0,1-5.637,0l-.939-.939a3.985,3.985,0,0,1,0-5.635L7.773,9.651a3.987,3.987,0,0,1,5.637,0l.939.94"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M11.53,5.893l4.227-4.226a3.985,3.985,0,0,1,5.637,0l.939.938a3.985,3.985,0,0,1,0,5.635l-6.106,6.107a3.985,3.985,0,0,1-5.637,0l-.939-.94"
    ]
    [ 
    ]
  ]

iconPlant :: forall p r i. Array (IProp r i) -> HTML p i
iconPlant attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "id") "Light"
  , attr (AttrName "viewBox") "0 0 24 24"
  ])
  [ elementNS ns (ElemName "defs")
    [ ]
    [ elementNS ns (ElemName "style")
      [ ]
      [ text """.cls-1{fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;}"""
      ]
    ], elementNS ns (ElemName "title")
    [ ]
    [ text """plant"""
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "cls-1"
    , attr (AttrName "d") "M17,8.5a5,5,0,0,0-5,5A5,5,0,0,0,17,8.5Z"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "cls-1"
    , attr (AttrName "d") "M5.5,15A6.5,6.5,0,0,1,12,21.5,6.5,6.5,0,0,1,5.5,15Z"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "cls-1"
    , attr (AttrName "d") "M18.5,15A6.5,6.5,0,0,0,12,21.5,6.5,6.5,0,0,0,18.5,15Z"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "cls-1"
    , attr (AttrName "d") "M7,8.5a5,5,0,0,1,5,5A5,5,0,0,1,7,8.5Z"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "cls-1"
    , attr (AttrName "d") "M12,.5a4.95,4.95,0,0,1,0,7A4.95,4.95,0,0,1,12,.5Z"
    ]
    [ 
    ], elementNS ns (ElemName "line")
    [ attr (AttrName "class") "cls-1"
    , attr (AttrName "x1") "12"
    , attr (AttrName "y1") "23.5"
    , attr (AttrName "x2") "12"
    , attr (AttrName "y2") "7.5"
    ]
    [ 
    ]
  ]

iconSmileySad1 :: forall p r i. Array (IProp r i) -> HTML p i
iconSmileySad1 attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "viewBox") "0 0 24 24"
  ])
  [ elementNS ns (ElemName "defs")
    [ ]
    [ elementNS ns (ElemName "style")
      [ ]
      [ text """.a{fill:none;stroke:currentColor;stroke-linecap:round;stroke-linejoin:round;}"""
      ]
    ], elementNS ns (ElemName "title")
    [ ]
    [ text """smiley-sad-1"""
    ], elementNS ns (ElemName "circle")
    [ attr (AttrName "class") "a"
    , attr (AttrName "cx") "12"
    , attr (AttrName "cy") "12"
    , attr (AttrName "r") "11.5"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M6.5,9.75a.25.25,0,1,1-.25.25.25.25,0,0,1,.25-.25"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M17.5,9.75a.25.25,0,1,0,.25.25.25.25,0,0,0-.25-.25"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "class") "a"
    , attr (AttrName "d") "M7.5,19a4.5,4.5,0,0,1,9,0"
    ]
    [ 
    ]
  ]

