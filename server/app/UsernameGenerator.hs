module UsernameGenerator where

import Data.Hashable (hash)
import qualified Data.Text as T
import Debug.Trace
import qualified System.Random as Random

data BookUsernameSource
  = Hitchhikers
  | Skyward {- RageOfDragons | -}
  | WayOfKings
  | Seveneves
  | -- | Hyperion
    Mistborn
  deriving (Enum)

sources :: [BookUsernameSource]
sources =
  [ Hitchhikers
    ..
  ]

usersForSource :: BookUsernameSource -> [T.Text]
usersForSource Hitchhikers =
  [ "Ford",
    "Zaphod",
    "Trillian",
    "Marvin",
    "Saltibartfast"
  ]
usersForSource Skyward =
  [ "Spensa",
    "Cobb",
    "Rig",
    "Jorgen",
    "Ironsides",
    "Freyja",
    "M-Bot"
  ]
-- usersForSource RageOfDragons =
-- [ "Spensa"
-- , ""
-- ]
usersForSource WayOfKings =
  [ "Kaladin",
    "Shallan",
    "Dalinar",
    "Szeth",
    "Adolin",
    "Jasnah"
  ]
usersForSource Seveneves =
  [ "Dinah",
    "Ivy",
    "Tekla",
    "Moira",
    "Camila",
    "Aïda"
  ]
usersForSource Mistborn =
  [ "Vin",
    "Kelsier",
    "Dockson",
    "Ham",
    "Breeze",
    "Clubs",
    "Spook",
    "Rashek",
    "Marsh",
    "Sazed",
    "Elend"
  ]

type RequestUuid = T.Text

type RecommendationUuid = T.Text

data UserGen = UserGen BookUsernameSource Random.StdGen

mkUsersGen :: RequestUuid -> UserGen
mkUsersGen u = UserGen Hitchhikers $ Random.mkStdGen (hash u)

-- nextUsername :: UserGen -> (T.Text, UserGen)
-- nextUsername (UserGen source gen) = do

-- instance Monad UserGen

genUsername :: RequestUuid -> Maybe RecommendationUuid -> T.Text
genUsername q c = do
  let i = fst $ Random.next $ Random.mkStdGen (hash q)
  let source = sources !! (i `mod` length sources)
  let j = maybe i (fst . Random.next . Random.mkStdGen . hash) c
  let usersList = usersForSource source
  usersList !! (j `mod` length usersList)
