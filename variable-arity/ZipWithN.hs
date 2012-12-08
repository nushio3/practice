{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                  ~ 2009.12.20
-- |
-- Module      :  Data.List.ZipWithN
-- Copyright   :  Copyright (c) 2009--2010 wren ng thornton
-- License     :  BSD3
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs, FunDeps,...)
--
-- Provides a polyvariadic 'map'/'zipWith' like the @map@ in Scheme.
-- For more details on this style of type hackery, see:
--
--    * Chung-chieh Shan, /A polyvariadic function of a non-regular/
--      /type (Int->)^N ([]^N e)->.../
--      <http://okmij.org/ftp/Haskell/polyvariadic.html#polyvartype-fn>
--
-- For alternative approaches to solving this problem, see also
--
--    * http://paczesiowa.blogspot.com/2010/03/generalized-zipwithn.html
--
--    * http://hackage.haskell.org/packages/archive/TypeCompose/latest/doc/html/Data-Zip.html
----------------------------------------------------------------
module Data.List.ZipWithN (ZipWithN(), zipWithN) where

-- TODO: add the extra hackery that lets us get rid of the fundeps,
-- if we can figure out how the heck it works.
-- 
-- | This class provides the necessary polymorphism. It is only
-- exported for the sake of giving type signatures.
--
-- Because we can't do functor composition without a lot of noise
-- from newtype wrappers, we use @gr@ and @kr@ to precompose the
-- direct/list functor with the reader functor and the return type.
class ZipWithN a gr kr | kr -> gr a where
    _zipWithN :: [a -> gr] -> [a] -> kr

instance ZipWithN a b [b] where
    _zipWithN = zipWith ($)

instance ZipWithN b gr kr => ZipWithN a (b -> gr) ([b] -> kr) where
    _zipWithN = (_zipWithN .) . zipWith ($)


-- | Polyadic version of 'map'/'zipWith'. The given type signature
-- isn't terribly helpful or intuitive. The /real/ type signature
-- is:
--
-- > zipWithN :: {forall a}^N. ({a->}^N  r) -> ({[a]->}^N  r)
--
-- Note that the @a@ type variables are meta and so are independent
-- from one another, despite being correlated in N across all
-- repetitions.
zipWithN :: (ZipWithN a gr kr) => (a -> gr) -> [a] -> kr
zipWithN = _zipWithN . repeat

----------------------------------------------------------------
----------------------------------------------------------- fin.
