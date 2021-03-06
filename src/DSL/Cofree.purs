module DSL.Cofree
  ( runExample
  , Run(..)
  , mkInterp
  , pair
  , cmds
  ) where

-- | (synchronous) interpreter for the `StoreDSL` using Cofree

import Prelude

import DSL.Types (Command(..), StoreDSL, User(..), addUser, changeName, getUsers)
import Data.Array as A
import Control.Comonad.Cofree (Cofree, explore, unfoldCofree)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (foldl, sequence_)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

newtype Run a = Run
    { addUser :: User -> a
    , remove :: Int ->  a
    , changeName :: Int -> String -> a
    -- getUsers could be just `Tuple (Array User) a`, but let's have a fancy function ;)
    , getUsers :: Unit -> Tuple (Array User) a
    , getUser :: Tuple User a
    , saveUser :: User -> a
    }

derive instance functorRun :: Functor Run

-- | interpreter's type
type Interp a = Cofree Run a

-- | create an interpreter with initial state
mkInterp :: Array User -> Interp (Array User)
mkInterp state = unfoldCofree id next state
  where
      addUser :: Array User -> User -> Array User
      addUser st = A.snoc st

      remove :: Array User -> Int -> Array User
      remove st uid = A.filter (\u -> (unwrap u).id /= uid) st

      changeName :: Array User -> Int -> String -> Array User
      changeName st uid name =
        let chname acu (User u) =
                if u.id /= uid
                    then A.snoc acu (User u)
                    else A.snoc acu (User u { name = name })
          in foldl chname [] st

      getUsers :: Array User -> Unit -> Tuple (Array User) (Array User)
      getUsers st = 
          let users = [User {id: 2, name: "Pierre"}, User {id: 3, name: "Diogo"}]
           in const $ Tuple users st

      getUser :: Array User -> Tuple User (Array User)
      getUser st = Tuple (User {id: 4, name: "Wojtek"}) st

      next :: Array User -> Run (Array User)
      next st = Run
        { addUser: addUser st
        , remove: remove st
        , changeName: changeName st
        , getUsers: getUsers st
        , getUser: getUser st
        , saveUser: const st
        }


-- | pairing between `Command (x -> y)` and `Run`
pair :: forall x y. Command (x -> y) -> Run x -> y
pair (Add u f) (Run interp) = f $ interp.addUser u
pair (Remove uid f) (Run interp) = f $ interp.remove uid
pair (ChangeName uid name f) (Run interp) = f $ interp.changeName uid name
pair (GetUsers f) (Run interp) = case interp.getUsers unit of
    Tuple users x -> f users x
pair (SaveUser user f) (Run interp) = f $ interp.saveUser user
pair (GetUser f) (Run interp) = (\(Tuple u x) -> f u x) interp.getUser

cmds :: StoreDSL (Array User -> Array User)
cmds = do
    users <- getUsers
    -- interp.getUser is adding users to the state
    sequence_ $ addUser <$> users
    changeName 1 "coot"

run :: StoreDSL (Array User -> Array User) -> Array User -> Array User
run cmds_ state = explore pair cmds_ $ mkInterp state

runExample:: forall e. Eff (console :: CONSOLE | e) Unit
runExample = do
    log $ show $ run cmds [User {id: 1, name: "Marcin"}]
