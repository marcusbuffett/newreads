module MockBooks where

hitchikersguide :: String
hitchikersguide = "386162"

exhalations :: String
exhalations = "41160292"

threebodyproblem :: String
threebodyproblem = "20518872"

fifthscience :: String
fifthscience = "41580260"

biff :: String
biff = "28881"

martian :: String
martian = "18007564"

guardsguards :: String
guardsguards = "64216"

ragedragons :: String
ragedragons = "41952489"

digitalminimalism :: String
digitalminimalism = "40672036"

thinklikeromanemperor :: String
thinklikeromanemperor = "39863499"

whywesleep :: String
whywesleep = "34466963"

goodtogo :: String
goodtogo = "45894059"

educated :: String
educated = "35133922"

knowmyname :: String
knowmyname = "50196744"

egoistheenemy :: String
egoistheenemy = "27036528"

peakexpertise :: String
peakexpertise = "26312997"

whenbreathair :: String
whenbreathair = "25899336"

childrenoftime :: String
childrenoftime = "25499718"

seveneves :: String
seveneves = "22816087"

righteousmind :: String
righteousmind = "11324722"

wayofkings :: String
wayofkings = "7235533"

nameofthewind :: String
nameofthewind = "186074"

hyperion :: String
hyperion = "77566"

creativityinc :: String
creativityinc = "18077903"

shoedog :: String
shoedog = "27220736"

furiesofcalderon :: String
furiesofcalderon = "29396"

deathlyhallows :: String
deathlyhallows = "136251"

allMockBooks :: [String]
allMockBooks =
  map
    ("/book/show/" ++)
    [ hitchikersguide,
      exhalations,
      threebodyproblem,
      fifthscience,
      biff,
      martian,
      guardsguards,
      ragedragons,
      digitalminimalism,
      thinklikeromanemperor,
      whywesleep,
      goodtogo,
      educated,
      knowmyname,
      egoistheenemy,
      peakexpertise,
      whenbreathair,
      childrenoftime,
      seveneves,
      righteousmind,
      wayofkings,
      nameofthewind,
      hyperion,
      creativityinc,
      shoedog,
      furiesofcalderon
    ]
