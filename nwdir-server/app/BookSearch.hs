{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BookSearch
  ( BookSearchEngine,
    initialBookSearchEngine,
    defaultSearchRankParameters,
    BookDocField (..),
    extractTerms,
    BookDoc (..),
    prefixes,
  )
where

import Data.Ix
import Data.SearchEngine
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec

type BookSearchEngine =
  SearchEngine
    BookDoc
    Text
    BookDocField
    BookDocFeature

data BookDocField
  = TitleField
  | AuthorField
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

data BookDocFeature
  = FeatureRatings
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

data BookDoc
  = BookDoc
      { docTitle :: Text,
        docAuthor :: Text,
        docNumRatings :: Int,
        docUuid :: Text
      }
  deriving (Show)

initialBookSearchEngine :: BookSearchEngine
initialBookSearchEngine =
  initSearchEngine bookSearchConfig defaultSearchRankParameters

bookSearchConfig ::
  SearchConfig BookDoc
    Text
    BookDocField
    BookDocFeature
bookSearchConfig =
  SearchConfig
    { documentKey = docUuid,
      extractDocumentTerms = extractTokens,
      transformQueryTerm = normaliseQueryToken,
      documentFeatureValue = documentFeatureValue
    }
  where
    extractTokens :: BookDoc -> BookDocField -> [Text]
    extractTokens doc TitleField =
      extractTerms (docTitle doc)
    extractTokens doc AuthorField =
      extractTerms (docAuthor doc)
    normaliseQueryToken :: Text -> BookDocField -> Text
    normaliseQueryToken tok =
      let tokFold = T.toCaseFold tok
       in \field -> case field of
            TitleField -> tokFold
            AuthorField -> tokFold
    documentFeatureValue doc feature = case feature of
      FeatureRatings -> (fromIntegral $ docNumRatings doc)

defaultSearchRankParameters :: SearchRankParameters BookDocField BookDocFeature
defaultSearchRankParameters =
  SearchRankParameters
    { paramK1,
      paramB,
      paramFieldWeights,
      paramFeatureWeights = const 0.5,
      paramFeatureFunctions = featureFunctions,
      paramResultsetSoftLimit = 100,
      paramResultsetHardLimit = 100,
      paramAutosuggestPrefilterLimit = 500,
      paramAutosuggestPostfilterLimit = 500
    }
  where
    paramK1 :: Float
    paramK1 = 1.2
    paramB :: BookDocField -> Float
    paramB TitleField = 0.75
    paramB AuthorField = 0.75
    paramFieldWeights :: BookDocField -> Float
    paramFieldWeights TitleField = 1.00
    paramFieldWeights AuthorField = 1.00
    featureFunctions feature = LogarithmicFunction 2

extractTerms :: Text -> [Text]
extractTerms str = filter (\w -> T.length w >= 3 && Set.notMember w stopWords) . (concatMap (prefixes . T.toLower) . T.splitOn " ") $ str

prefixes :: Text -> [Text]
prefixes "" = []
prefixes str = str : prefixes (T.dropEnd 1 str)

stopWords :: Set Term
stopWords = Set.fromList ["the", "and", "for", "you"]
