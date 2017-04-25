module DSL.Utils where

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, tail)
import Control.Monad.Aff (Aff)
import Control.Monad.Free (Free, runFreeM)
import Control.Monad.State (StateT(..), runStateT)
import Data.Tuple (Tuple(..))
import Prelude (map, class Functor, ($))

exploreAff
  :: forall f g a b eff
  .  Functor f
  => Functor g
  => (forall x y. f (x -> y) -> g x -> Aff eff y)
  -> Free f (a -> b)
  -> Cofree g a
  -> Aff eff b
exploreAff pair m w =
  map eval $ runStateT (runFreeM step m) w
  where
    step :: f (Free f (a -> b)) -> StateT (Cofree g a) (Aff eff) (Free f (a -> b))
    step ff = StateT \cof -> pair (map Tuple ff) (tail cof)

    eval :: forall x y. Tuple (x -> y) (Cofree g x) -> y
    eval (Tuple f cof) = f (extract cof)
