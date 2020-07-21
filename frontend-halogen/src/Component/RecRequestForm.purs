module Component.RecRequestForm where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Formless as F
import Data.Symbol (SProxy(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Dog
  = { name :: String, age :: Age }

newtype Age
  = Age Int

derive instance newtypeAge :: Newtype Age _

instance showAge :: Show Age where
  show = show <<< unwrap

data AgeError
  = TooLow
  | TooHigh
  | InvalidInt

newtype DogForm r f
  = DogForm
  ( r
      --          error    input  output
      ( name :: f Void String String
      , age :: f AgeError String Age
      )
  )

derive instance newtypeDogForm :: Newtype (DogForm r f) _

input :: forall m. Monad m => F.Input' DogForm m
input =
  { initialInputs: Nothing -- same as: Just (F.wrapInputFields { name: "", age: "" })
  , validators:
    DogForm
      { name: F.noValidation
      , age:
        F.hoistFnE_ \str -> case Int.fromString str of
          Nothing -> Left InvalidInt
          Just n
            | n < 0 -> Left TooLow
            | n > 30 -> Left TooHigh
            | otherwise -> Right (Age n)
      }
  }

spec :: forall input m. Monad m => F.Spec' DogForm Dog input m
spec = F.defaultSpec { render = render, handleEvent = F.raiseResult }
  where
  render st@{ form } =
    HH.form_
      [ HH.input
          [ HP.value $ F.getInput _name form
          , HE.onValueInput $ Just <<< F.set _name
          ]
      , HH.input
          [ HP.value $ F.getInput _age form
          , HE.onValueInput $ Just <<< F.setValidate _age
          ]
      , HH.text case F.getError _age form of
          Nothing -> ""
          Just InvalidInt -> "Age must be an integer"
          Just TooLow -> "Age cannot be negative"
          Just TooHigh -> "No dog has lived past 30 before"
      , HH.button
          [ HE.onClick \_ -> Just F.submit ]
          [ HH.text "Submit" ]
      ]
    where
    _name = SProxy :: SProxy "name"

    _age = SProxy :: SProxy "age"

component = F.component (const input) spec
