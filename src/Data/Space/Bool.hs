{-# LANGUAGE TypeOperators #-}

module Data.Space.Bool where

import Control.Comonad.Trans.Adjoint as W
import Control.Comonad.Trans.Env
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Functor.Adjunction
import Data.Map as Map
import Data.Set as Set
import Data.Universe
import GHC.Generics

{-
type Cell a = W.Adjoint (Env (TVar Bool)) (Reader (TVar Bool)) a

type DataDendrit = Map (Set Bool) Bool

type Dendrit a = W.Adjoint (Env (TVar DataDendrit)) (Reader (TVar DataDendrit )) a

type DendritCell a =
  W.Adjoint
    (Env (TVar DataDendrit) :.: Env (TVar Word8))
    (Reader (TVar Word8) :.: Reader (TVar DataDendrit))
    a

type SpaceBool a = Universe2 (Maybe (DendritCell a))

genEmptySpace :: SpaceBool a
genEmptySpace = Universe2 $ makeUniverse id id $ makeUniverse id id Nothing

initSpace :: Int -> StateT (SpaceBool ()) IO ()
initSpace i sb = do
  mapM (\(x,y)-> do
    return $
    ) (liftA2 (,) [-i..i] [-i..i])
-}
