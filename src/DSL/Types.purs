module DSL.Types 
  ( User(..)
  , Action(..)
  , Command
  , StoreDSL
  ) where

import Prelude

import Control.Monad.Free (Free)
import Data.Newtype (class Newtype, over)
import Data.Coyoneda (Coyoneda(..))

newtype User = User
    { id :: Int
    , name :: String
    }

instance showUser :: Show User where
    show (User u) = "User " <> show u.name

derive instance newtypeUser :: Newtype User _

-- | DSL commands (crud)
-- | We want a functor of this shape
-- | ```purescript
-- | data Command a = Add User a
-- |                | Remove Int a
-- |                | ChangeName Int String a
-- |                | GetUsers a
-- |                | SaveUser User a
-- |
-- | instance functorCommand :: Functor Command where
-- |     map f (Add u a) = Add u (f a)
-- |     map f (Remove uid a) = Remove uid (f a)
-- |     map f (ChangeName uid name a) = ChangeName uid name (f a)
-- |     map f (GetUsers a) = GetUsers (f a)
-- |     map f (SaveUser u a) = SaveUser u (f a)
-- | ```
-- | We can get it for free (indeed using the free functor construction) with `Coyoneda`
data Action a
  = Add User a
  | Remove Int a
  | ChangeName Int String a
  | GetUsers (Array User -> a)
  | SaveUser User a

type Command = Coyoneda Action

-- | DSL
type StoreDSL a = Free Command a
