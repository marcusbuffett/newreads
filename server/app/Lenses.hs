module Lenses where

import           Control.Lens
import           Control.Lens.Internal.FieldTH
import qualified Data.Char                     as C
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Debug.Trace
import           Language.Haskell.TH

-- REVIEW: this file unused, no need to review
transformBaseName :: Name -> Name
transformBaseName s = mkName . camelCase . removePrefix . show $ s
  where
    removePrefix :: String -> String
    removePrefix =
      dropWhile (\c -> c == '_' || C.isLower c) .
      dropWhile ((==) '.') . dropWhile ((/=) '.')
    camelCase :: String -> String
    camelCase (x:xs) = C.toLower x : xs
    camelCase []     = []

myMakeLenses :: Name -> DecsQ
myMakeLenses =
  makeLensesWith $
  classyRules &
  lensField .~ \_ _ name ->
    [MethodName (mkName "Blah") (mkName $ nameBase $ transformBaseName name)]
