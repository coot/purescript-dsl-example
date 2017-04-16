module DSL.Types 
  ( User(..)
  , Command(..)
  , StoreDSL
  , addUser
  , removeUser
  , changeName
  , getUsers
  ) where

import Prelude

import Control.Monad.Free (Free, liftF)
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

addUser :: User -> StoreDSL Unit
addUser u = liftF (Add u unit)

removeUser :: Int -> StoreDSL Unit
removeUser uid = liftF (Remove uid unit)

changeName :: Int -> String -> StoreDSL Unit
changeName uid name = liftF (ChangeName uid name unit)

getUsers :: StoreDSL (Array User)
getUsers = liftF $ GetUsers id
