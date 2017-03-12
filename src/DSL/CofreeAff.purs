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

import Prelude (class Functor, Unit, bind, id, map, pure, show, unit, ($), (/=), (<$>), (<<<))
import Data.Array as A
import Control.Comonad.Cofree (Cofree)
import Control.Monad.Aff (Aff, later, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Free (Free, liftF)
import Data.Foldable (foldl, sequence_)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

import DSL.Types
import DSL.Utils (exploreInAff, coiter)

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
    , getUsers :: Unit -> Aff eff (Tuple (Array User) a)
    , saveUser :: User -> Aff eff a
    }

instance functorCoComamnd :: Functor (RunAff eff) where
    map f (RunAff { addUser, remove, changeName, getUsers, saveUser }) = RunAff
        { addUser: \u -> f <$> addUser u
        , remove: \uid -> f <$> remove uid
        , changeName: \uid name -> f <$> changeName uid name
        , getUsers: \u -> (map f) <$> getUsers u
        , saveUser: (map f) <<< saveUser
        }

-- | interpreter's type
type AffInterp eff a = Cofree (RunAff eff) a

-- create the interpreter with initial state
mkAffInterp :: forall eff. Array User -> AffInterp eff (Array User)
mkAffInterp state = coiter next state
  where
      addUser :: Array User -> User -> Aff eff (Array User)
      addUser state u = pure $ A.snoc state u

      remove :: Array User -> Int -> Aff eff (Array User)
      remove state uid = pure $ A.filter (\u -> (unwrap u).id /= uid) state

      changeName :: Array User -> Int -> String -> Aff eff (Array User)
      changeName state uid name =
        let
            chname :: Array User -> User -> Array User
            chname acu (User u) =
                if u.id /= uid
                    then A.snoc acu (User u)
                    else A.snoc acu (User u { name = name })
            in pure $ foldl chname [] state

      getUsers :: Array User -> Unit -> Aff eff (Tuple (Array User) (Array User))
      getUsers state =
        let users = [User {id: 2, name: "Pierre"}, User {id: 3, name: "Diogo"}]
         in \_ -> later (pure $ Tuple users state)

      saveUser :: Array User -> User -> Aff eff (Array User)
      saveUser state user = pure state

      next :: Array User -> RunAff eff (Array User)
      next state = RunAff
        { addUser: addUser state
        , remove: remove state
        , changeName: changeName state
        , getUsers: getUsers state
        , saveUser: saveUser state
        }

pairInAff :: forall eff x y. Command (x -> y) -> RunAff eff x -> Aff eff y
pairInAff (Add u f) (RunAff interp) = f <$> interp.addUser u
pairInAff (Remove uid f) (RunAff interp) = f <$> interp.remove uid
pairInAff (ChangeName uid name f) (RunAff interp) = f <$> interp.changeName uid name
pairInAff (GetUsers f) (RunAff interp) = (\(Tuple users x) -> f users x) <$> interp.getUsers unit
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
      $ exploreInAff pairInAff task (mkAffInterp [User {id: 1, name: "Marcin"}])
    log "done"
