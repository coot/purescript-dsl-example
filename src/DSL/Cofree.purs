module DSL.Cofree
  ( runExample
  , addUser
  , removeUser
  , changeName
  , getUsers
  , Run(..)
  , mkInterp
  , pair
  , cmds
  ) where

-- | (synchronous) interpreter for the `StoreDSL` using Cofree

import Prelude

import DSL.Types
import Data.Array as A
import Control.Comonad.Cofree (Cofree, explore, unfoldCofree)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Free (liftF)
import Data.Foldable (foldl, sequence_)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

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
    -- getUsers could be just `Tuple (Array User) a`, but let's have a fancy function ;)
    , getUsers :: Unit -> Tuple (Array User) a
    , saveUser :: User -> a
    }

instance functorRun :: Functor Run where
    map f (Run { addUser, remove, changeName, getUsers, saveUser }) = Run
        { addUser: \u -> f $ addUser u
        , remove: \uid -> f $ remove uid
        , changeName: \uid name -> f $ changeName uid name
        , getUsers: (map f) <$> getUsers
        , saveUser: f <<< saveUser
        }

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

      next :: Array User -> Run (Array User)
      next state = Run
        { addUser: addUser state
        , remove: remove state
        , changeName: changeName state
        , getUsers: getUsers state
        , saveUser: const state
        }


-- | pairing between `Command (x -> y)` and `Run`
pair :: forall x y. Command (x -> y) -> Run x -> y
pair (Add u f) (Run interp) = f $ interp.addUser u
pair (Remove uid f) (Run interp) = f $ interp.remove uid
pair (ChangeName uid name f) (Run interp) = f $ interp.changeName uid name
pair (GetUsers f) (Run interp) = case interp.getUsers unit of
    Tuple users x -> f users x
pair (SaveUser user f) (Run interp) = f $ interp.saveUser user

cmds :: StoreDSL (Array User -> Array User)
cmds = do
    users <- getUsers
    -- interp.getUser is adding users to the state
    sequence_ $ addUser <$> users
    changeName 1 "coot"
    pure id

run :: StoreDSL (Array User -> Array User) -> Array User -> Array User
run cmds state = explore pair cmds $ mkInterp state

runExample:: forall e. Eff (console :: CONSOLE | e) Unit
runExample = do
    log $ show $ run cmds [User {id: 1, name: "Marcin"}]
