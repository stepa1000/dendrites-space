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

type Space f g a = W.AdjointT f g Universe2 a

makeSpace :: (Monad m) => f () -> g b -> g b -> a -> (a -> m (Maybe (g b, a))) -> m (Space f g b)
makeSpace fa fgNil gCentr a mg = do
  ll <- fixF a
  rl <- fixF a
  ll' <- fixF a
  rl' <- fixF a
  llu <- mapM genU ll'
  lru <- mapM genU rl'
  return $ W.AdjointT $ (const $ Universe2 $ Universe llu (Universe ll gCentr rl) lru) <$> fa
  where
    genU g = do
      ll <- fixF a
      rl <- fixF a
      return $ Universe ll g rl
    fixF a = do
      mgb <- mg a
      case mgb of
        (Just (gb, a2)) -> do
          gbl <- fixF a2
          return $ gb : gbl
        Nothing -> return $ repeat fg

upS :: (Adjunction f g, Comonad w) => Space f g a -> Space f g a
upS = hoistWAdj up
