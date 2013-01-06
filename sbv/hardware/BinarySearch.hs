-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SBV.Examples.Optimization.Binary
-- Copyright   :  (c) Levent Erkok, Takayuki Muranushi
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com, muranushi@gmail.com
-- Stability   :  experimental
--
--

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module BinarySearch where

import Data.SBV
import Text.Printf

type BSPred a = a -> Predicate
type BSBounds a = SMTConfig -> BSPred a -> IO (a,a)
type BSNext a = a -> a -> Maybe a

data BSOpts a =
  BSOpts
  { bsBounds :: BSBounds a
  , bsNext :: BSNext a
  }

class DefaultBSOpts a where
  defaultBSOpts :: BSOpts a

bsBoundsBounded :: (Bounded a) => BSBounds a
bsBoundsBounded _cfg _pred0 = return (minBound, maxBound)

bsBoundsSearch :: (Num a, Show a) => BSBounds a
bsBoundsSearch cfg pred0 = do
  lower <- goDown (-1)
  upper <- goUp 1
  return (lower, upper)
    where
      goUp x = do
        (SatResult ans) <- satWith cfg $ pred0 x
        case ans of
          Satisfiable _ _ -> return x
          _               -> goUp $ 2*x
      goDown x = do
        (SatResult ans) <- satWith cfg $ pred0 x
        case ans of
          Unsatisfiable  _ -> return x
          _                -> goDown $ 2*x



bsNextIntegral :: (Integral a) => BSNext a
bsNextIntegral lower upper
  | upper <= lower+1 = Nothing
  | otherwise        =
        Just $ ((lower+1) `div` 2) + (upper `div` 2)

bsNextFractional :: (Ord a, Fractional a) => BSNext a
bsNextFractional lower upper
  | upper <= lower+1e-16 = Nothing
  | otherwise = Just $ (lower+upper)/2


instance DefaultBSOpts Int32 where
  defaultBSOpts = BSOpts
    { bsBounds = bsBoundsBounded
    , bsNext   = bsNextIntegral
    }

instance DefaultBSOpts Int64 where
  defaultBSOpts = BSOpts
    { bsBounds = bsBoundsBounded
    , bsNext   = bsNextIntegral
    }

instance DefaultBSOpts Word32 where
  defaultBSOpts = BSOpts
    { bsBounds = bsBoundsBounded
    , bsNext   = bsNextIntegral
    }

instance DefaultBSOpts Word64 where
  defaultBSOpts = BSOpts
    { bsBounds = bsBoundsBounded
    , bsNext   = bsNextIntegral
    }



instance DefaultBSOpts Integer where
  defaultBSOpts = BSOpts
    { bsBounds = bsBoundsSearch
    , bsNext = bsNextIntegral }

instance DefaultBSOpts Rational where
  defaultBSOpts = BSOpts
    { bsBounds = bsBoundsSearch
    , bsNext = bsNextFractional }


binarySearchWith ::
  Show a               
  => SMTConfig
  -> BSOpts a
  -> BSPred a
  -> IO (a, Maybe SatResult)
binarySearchWith cfg opts pred0 = do
  (lower, upper) <- bsBounds opts cfg pred0
  go Nothing lower upper

  where
    go lastAns lower upper = do
      let quit = do
               return (upper, lastAns)
      case bsNext opts lower upper of
        Nothing           -> quit
        (Just middle) -> do
          ret@(SatResult ans) <- satWith cfg $ pred0 middle
          case ans of
            Satisfiable _ _ -> go (Just ret) lower middle
            Unsatisfiable _ -> go lastAns middle upper
            _               -> quit
