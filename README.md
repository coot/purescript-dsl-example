# A simple crud DSL with an interpreter written as cofree commonad in Purescript

The store type is simply
```purescript
type Store = Array User
```

where
```purescript
newtype User = User
    { id :: Int
    , name :: String
    }
```

There are four commands in the DSL:
```purescript
data Command a = Add User a
               | Remove Int a
               | ChangeName Int String a
               | GetUsers (Array User -> a)
               | SaveUser User a
```

The type of the DSL is
```purescript
type StoreDSL a = Free Command a
```

There are two interpreters
* synchronous one
  ```purescript
  newtype Run a = Run
      { addUser :: User -> a
      , remove :: Int ->  a
      , changeName :: Int -> String -> a
      , getUsers :: Unit -> Tuple (Array User) a
      , saveUser :: User -> a
      }

  type Interp a = Cofree Run a
  ```

  The pairing is given by
  ```purescript
  pair :: forall x y. Command (x -> y) -> Run x -> y
  ```

  We pair the `Free` and `Cofree` using the `explore` function from `Control.Comonad.Cofree` module.

* asynchronous one with computations in the `Aff` monad
  ```purescript
  newtype RunAff eff a = RunAff
      { addUser :: User -> Aff eff a
      , remove :: Int ->  Aff eff a
      , changeName :: Int -> String -> Aff eff a
      , getUsers :: Unit -> Aff eff (Tuple (Array User) a)
      , saveUser :: User -> Aff eff a
      }

  type AffInterp eff a = Cofree (RunAff eff) a
  ```

  The pairing is given by
  ```purescript
  pairInAff :: forall eff x y. Command (x -> y) -> RunAff eff x -> Aff eff y
  ```

  Here we pair using a custom `exploreInAff` function (defined in `DSL.Utils`.
