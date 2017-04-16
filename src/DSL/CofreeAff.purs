module DSL.CofreeAff 
  ( runAffExample
  , RunAff(..)
  , AffInterp
  , mkAffInterp
  , pairInAff
  ) where

-- | (asynchronous) interpreter for the `StoreDSL` using Cofree running
-- | commputations inside Aff monad

import Prelude

import Data.Array as A
import Data.Time.Duration (Milliseconds(..))
import Control.Comonad.Cofree (Cofree, unfoldCofree)
import Control.Monad.Aff (Aff, delay, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DSL.Types (Command(..), StoreDSL, User(..), addUser, removeUser, changeName, getUsers)
import DSL.Utils (exploreAff)
import Data.Foldable (foldl, sequence_)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))


newtype RunAff eff a = RunAff
    { addUser :: User -> Aff eff a
    , remove :: Int ->  Aff eff a
    , changeName :: Int -> String -> Aff eff a
    , getUsers :: Aff eff (Tuple (Array User) a)
    , saveUser :: User -> Aff eff a
    }

derive instance functorRunAff :: Functor (RunAff eff)

-- | interpreter's type
type AffInterp eff a = Cofree (RunAff eff) a

-- create the interpreter with initial state
mkAffInterp :: forall eff. Array User -> AffInterp eff (Array User)
mkAffInterp state = unfoldCofree id next state
  where
      addUser :: Array User -> User -> Aff eff (Array User)
      addUser st u = do
        delay $ Milliseconds 0.0
        pure $ A.snoc st u

      remove :: Array User -> Int -> Aff eff (Array User)
      remove st uid = do
        delay $ Milliseconds 0.0
        pure (A.filter (\user -> (unwrap user).id /= uid) st)

      changeName :: Array User -> Int -> String -> Aff eff (Array User)
      changeName st uid name =
        let
            chname :: Array User -> User -> Array User
            chname acu (User u) =
                if u.id /= uid
                    then A.snoc acu (User u)
                    else A.snoc acu (User u { name = name })
            in do
              delay $ Milliseconds 0.0
              pure (foldl chname [] st)

      getUsers :: Array User -> Aff eff (Tuple (Array User) (Array User))
      getUsers st =
        let users = [User {id: 2, name: "Pierre"}, User {id: 3, name: "Diogo"}]
         in do
           delay $ Milliseconds 0.0
           pure $ Tuple users st

      saveUser :: Array User -> User -> Aff eff (Array User)
      saveUser st user = do
        delay $ Milliseconds 0.0
        pure st

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

cmds :: StoreDSL (Array User -> Array User)
cmds = do
    users <- getUsers
    -- interp.getUser is adding users to the state
    sequence_ $ addUser <$> users
    changeName 1 "coot"

    pure id

run :: forall eff. StoreDSL (Array User -> Array User) -> Array User -> Aff eff (Array User)
run cmds state = exploreAff pairInAff cmds $ mkAffInterp state

runAffExample :: forall e. Eff (console :: CONSOLE | e) Unit
runAffExample = do
    _ <- runAff 
      (\_ -> log "ups...")
      (\users -> log $ show users)
      $ run cmds [User {id: 1, name: "Marcin"}]
    log "done"
