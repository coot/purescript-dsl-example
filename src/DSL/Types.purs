module DSL.Types 
  ( User(..)
  , Command(..)
  , StoreDSL
  ) where

import Prelude

import Control.Monad.Free (Free)
import Data.Newtype (class Newtype)

newtype User = User
    { id :: Int
    , name :: String
    }

instance showUser :: Show User where
    show (User u) = "User " <> show u.name

derive instance newtypeUser :: Newtype User _

-- | DSL commands (crud)
data Command a = Add User a
               | Remove Int a
               | ChangeName Int String a
               | GetUsers (Array User -> a)
               | SaveUser User a

derive instance functorCommand :: Functor Command

-- | DSL
type StoreDSL a = Free Command a
