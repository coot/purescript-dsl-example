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
import Data.Coyoneda (Coyoneda(..), liftCoyoneda, unCoyoneda)
import Data.Exists (runExists)
import Data.Foldable (foldl, sequence_)
import Data.Identity (Identity(..))
import Data.Newtype (wrap, unwrap)
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

addUser :: User -> StoreDSL Unit
addUser u = liftF $ Command $ liftCoyoneda (Add u unit)

removeUser :: Int -> StoreDSL Unit
removeUser uid = liftF $ Command $ liftCoyoneda (Remove uid unit)

changeName :: Int -> String -> StoreDSL Unit
changeName uid name = liftF $ Command $ liftCoyoneda (ChangeName uid name unit)

getUsers :: StoreDSL (Array User)
getUsers = liftF $ Command $ liftCoyoneda (GetUsers id)

newtype Run a = Run
    { addUser :: User -> a
    , remove :: Int ->  a
    , changeName :: Int -> String -> a
    , getUsers :: Tuple (Array User) a
    , saveUser :: User -> a
    }

instance functorRun :: Functor Run where
    map f (Run { addUser, remove, changeName, getUsers, saveUser }) = Run
        { addUser: \u -> f $ addUser u
        , remove: \uid -> f $ remove uid
        , changeName: \uid name -> f $ changeName uid name
        , getUsers: f <$> getUsers
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

      getUsers :: Array User -> Tuple (Array User) (Array User)
      getUsers st = 
          let users = [User {id: 2, name: "Pierre"}, User {id: 3, name: "Diogo"}]
           in Tuple users st

      next :: Array User -> Run (Array User)
      next state = Run
        { addUser: addUser state
        , remove: remove state
        , changeName: changeName state
        , getUsers: getUsers state
        , saveUser: const state
        }


-- | pairing between `Command (x -> y)` and `Run`
pair :: forall x y. Command (x ->y) -> Run x -> y
pair (Command c) r = pairAction (unCoyoneda unPack c) r
  where
    unPack :: forall i. (i -> x -> y) -> Action i -> Action (x -> y)
    unPack k ai = case ai of
               Add u i -> Add u (\x -> k i x)
               Remove id i -> Remove id (\x -> k i x)
               ChangeName id n i -> ChangeName id n (\x -> k i x)
               GetUsers i -> GetUsers (k <<< i)
               SaveUser u i -> SaveUser u (\x -> k i x)

    pairAction :: forall x y. Action (x -> y) -> Run x -> y
    pairAction (Add u f) (Run interp) = f $ interp.addUser u
    pairAction (Remove uid f) (Run interp) = f $ interp.remove uid
    pairAction (ChangeName uid name f) (Run interp) = f $ interp.changeName uid name
    pairAction (GetUsers f) (Run interp) = case interp.getUsers of
                                             Tuple users x -> f users x
    pairAction (SaveUser user f) (Run interp) = f $ interp.saveUser user

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
