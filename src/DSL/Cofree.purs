module DSL.Cofree
  ( runExample
  , addUser
  , removeUser
  , changeName
  , getUsers
  , Run(..)
  , mkInterp
  , pair
  , task
  ) where

-- | (synchronous) interpreter for the `StoreDSL` using Cofree

import Prelude (class Functor, Unit, bind, id, map, pure, show, unit, ($), (/=), (<$>), (<<<))
import Control.Comonad.Cofree (Cofree, explore)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Free (liftF)
import Data.Array as A
import Data.Foldable (foldl, sequence_)
import Data.Tuple (Tuple(..))
import Data.Newtype (unwrap)

import DSL.Types 
import DSL.Utils (coiter)

addUser :: User -> StoreDSL Unit
addUser u = liftF (Add u unit)

removeUser :: Int -> StoreDSL Unit
removeUser uid = liftF (Remove uid unit)

changeName :: Int -> String -> StoreDSL Unit
changeName uid name = liftF (ChangeName uid name unit)

getUsers :: StoreDSL (Array User)
getUsers = liftF $ GetUsers id

newtype Run a = Run
    { addUser :: User -> a
    , remove :: Int ->  a
    , changeName :: Int -> String -> a
    , getUsers :: Unit -> Tuple (Array User) a
    , saveUser :: User -> a
    }

instance functorRun :: Functor Run where
    map f (Run { addUser, remove, changeName, getUsers, saveUser }) = Run
        { addUser: \u -> f $ addUser u
        , remove: \uid -> f $ remove uid
        , changeName: \uid name -> f $ changeName uid name
        , getUsers: map (map f) getUsers
        , saveUser: f <<< saveUser
        }

-- | interpreters type
type Interp a = Cofree Run a

-- | create the interpreter with initial state
mkInterp :: Array User -> Interp (Array User)
mkInterp = coiter next
  where
      next state = Run
        { addUser: \u -> A.snoc state u
        , remove: \uid -> A.filter (\u -> (unwrap u).id /= uid) state
        , changeName: \uid name ->
            let chname acu (User u) =
                    if u.id /= uid
                        then A.snoc acu (User u)
                        else A.snoc acu (User u { name = name })
             in foldl chname [] state
        , getUsers: 
            let users = [User {id: 2, name: "Pierre"}, User {id: 3, name: "Diogo"}]
             in \_ -> Tuple users state
        , saveUser: \user -> state
        }


pair :: forall x y. Command (x -> y) -> Run x -> y
pair (Add u f) (Run interp) = f $ interp.addUser u
pair (Remove uid f) (Run interp) = f $ interp.remove uid
pair (ChangeName uid name f) (Run interp) = f $ interp.changeName uid name
pair (GetUsers f) (Run interp) = case interp.getUsers unit of
    Tuple users x -> f users x
pair (SaveUser user f) (Run interp) = f $ interp.saveUser user

task :: StoreDSL (Array User -> Array User)
task = do
    users <- getUsers
    -- interp.getUser is adding users to the state
    sequence_ $ addUser <$> users
    changeName 1 "coot"
    pure id

runExample:: forall e. Eff (console :: CONSOLE | e) Unit
runExample = do
    log $ show $ explore pair task (mkInterp [User {id: 1, name: "Marcin"}])
