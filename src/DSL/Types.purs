module DSL.Types 
  ( User(..)
  , Command(..)
  , StoreDSL
  , addUser
  , removeUser
  , changeName
  , getUsers
  , getUser
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
               | GetUser (User -> a)
               | SaveUser User a

derive instance functorCommand :: Functor Command

-- | DSL
type StoreDSL a = Free Command a

addUser :: User -> StoreDSL (Array User -> Array User)
addUser u = liftF (Add u id)

removeUser :: Int -> StoreDSL (Array User -> Array User)
removeUser uid = liftF (Remove uid id)

changeName :: Int -> String -> StoreDSL (Array User -> Array User)
changeName uid name = liftF (ChangeName uid name id)

getUsers :: StoreDSL (Array User)
getUsers = liftF $ GetUsers id

getUser :: StoreDSL User
getUser = liftF $ GetUser id
