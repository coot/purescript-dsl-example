module DSL.CofreeAff 
  ( runAffExample
  , addUser
  , removeUser
  , changeName
  , getUsers
  , RunAff(..)
  , AffInterp
  , mkAffInterp
  , pairInAff
  ) where

-- | (asynchronous) interpreter for the `StoreDSL` using Cofree running
-- | commputations inside Aff monad

import Data.Array as A
import Control.Comonad.Cofree (Cofree, unfoldCofree)
import Control.Monad.Aff (Aff, later, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Free (liftF)
import DSL.Types (Command(..), StoreDSL, User(..))
import DSL.Utils (exploreAff)
import Data.Foldable (foldl, sequence_)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Prelude (class Functor, Unit, bind, id, map, pure, show, unit, ($), (/=), (<$>), (<<<))

addUser :: User -> StoreDSL Unit
addUser u = liftF (Add u unit)

removeUser :: Int -> StoreDSL Unit
removeUser uid = liftF (Remove uid unit)

changeName :: Int -> String -> StoreDSL Unit
changeName uid name = liftF (ChangeName uid name unit)

getUsers :: StoreDSL (Array User)
getUsers = liftF $ GetUsers id

newtype RunAff eff a = RunAff
    { addUser :: User -> Aff eff a
    , remove :: Int ->  Aff eff a
    , changeName :: Int -> String -> Aff eff a
    , getUsers :: Aff eff (Tuple (Array User) a)
    , saveUser :: User -> Aff eff a
    }

instance functorRunAff :: Functor (RunAff eff) where
    map f (RunAff { addUser, remove, changeName, getUsers, saveUser }) = RunAff
        { addUser: \u -> f <$> addUser u
        , remove: \uid -> f <$> remove uid
        , changeName: \uid name -> f <$> changeName uid name
        , getUsers: (map f) <$> getUsers
        , saveUser: (map f) <<< saveUser
        }

-- | interpreter's type
type AffInterp eff a = Cofree (RunAff eff) a

-- create the interpreter with initial state
mkAffInterp :: forall eff. Array User -> AffInterp eff (Array User)
mkAffInterp state = unfoldCofree state id next
  where
      addUser :: Array User -> User -> Aff eff (Array User)
      addUser st u = later $ pure $ A.snoc st u

      remove :: Array User -> Int -> Aff eff (Array User)
      remove st uid = later $ pure (A.filter (\user -> (unwrap user).id /= uid) st)

      changeName :: Array User -> Int -> String -> Aff eff (Array User)
      changeName st uid name =
        let
            chname :: Array User -> User -> Array User
            chname acu (User u) =
                if u.id /= uid
                    then A.snoc acu (User u)
                    else A.snoc acu (User u { name = name })
            in later $ pure (foldl chname [] st)

      getUsers :: Array User -> Aff eff (Tuple (Array User) (Array User))
      getUsers st =
        let users = [User {id: 2, name: "Pierre"}, User {id: 3, name: "Diogo"}]
         in later (pure $ Tuple users st)

      saveUser :: Array User -> User -> Aff eff (Array User)
      saveUser st user = later $ pure st

      next :: Array User -> RunAff eff (Array User)
      next st = RunAff
        { addUser: addUser st
        , remove: remove st
        , changeName: changeName st
        , getUsers: getUsers st
        , saveUser: saveUser st
        }

pairInAff :: forall eff x y. Command (x -> y) -> RunAff eff x -> Aff eff y
pairInAff (Add u f) (RunAff interp) = f <$> interp.addUser u
pairInAff (Remove uid f) (RunAff interp) = f <$> interp.remove uid
pairInAff (ChangeName uid name f) (RunAff interp) = f <$> interp.changeName uid name
pairInAff (GetUsers f) (RunAff interp) = (\(Tuple users x) -> f users x) <$> interp.getUsers
pairInAff (SaveUser user f) (RunAff interp) = f <$> interp.saveUser user

task :: StoreDSL (Array User -> Array User)
task = do
    users <- getUsers
    -- interp.getUser is adding users to the state
    sequence_ $ addUser <$> users
    changeName 1 "coot"

    pure id

runAffExample :: forall e. Eff (console :: CONSOLE | e) Unit
runAffExample = do
    runAff 
      (\_ -> log "ups...")
      (\users -> log $ show users)
      $ exploreAff pairInAff task (mkAffInterp [User {id: 1, name: "Marcin"}])
    log "done"
