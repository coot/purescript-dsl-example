module Main where

import DSL.Cofree (runExample)
import DSL.CofreeAff (runAffExample)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Prelude (Unit, bind)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "\nCofree"
  runExample
  log "\nCofreeAff"
  runAffExample
