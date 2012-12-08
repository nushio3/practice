{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleContexts
           , FlexibleInstances
           , OverlappingInstances
           , TypeFamilies
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

import qualified Data.Key as K


-- TODO: add the extra hackery that lets us get rid of the fundeps,
-- if we can figure out how the heck it works.
--
-- | This class provides the necessary polymorphism. It is only
-- exported for the sake of giving type signatures.
--
-- Because we can't do functor composition without a lot of noise
-- from newtype wrappers, we use @gr@ and @kr@ to precompose the
-- direct/list functor with the reader functor and the return type.
class K.Zip f => ZipWithN f a gr kr | kr -> a gr where
    _zipWithN :: f (a -> gr) -> f a -> kr

instance (K.Zip f, f b ~ fb) => ZipWithN f a b fb where
    _zipWithN = K.zipWith ($)

instance (K.Zip f, ZipWithN f b gr kr) =>
         ZipWithN f a (b -> gr) (f b -> kr) where
    _zipWithN = (_zipWithN .) . K.zipWith ($)


-- | Polyadic version of 'map'/'zipWith'. The given type signature
-- isn't terribly helpful or intuitive. The /real/ type signature
-- is:
--
-- > zipWithN :: {forall a}^N. ({a->}^N  r) -> ({[a]->}^N  r)
--
-- Note that the @a@ type variables are meta and so are independent
-- from one another, despite being correlated in N across all
-- repetitions.
--
-- >>> let f i c d = c : show (i::Int) ++ " " ++ show (d::Double)
-- >>> zipWithN f [1 :: Int ..] "hoge" [1 :: Double ..] :: [String]
-- ["h1 1.0","o2 2.0","g3 3.0","e4 4.0"]
--
-- >>> import qualified Data.Vector as V
-- >>> instance K.Zip V.Vector where zipWith = V.zipWith
-- >>> let xs = V.fromList [1..10] :: V.Vector Int
-- >>> zipWithN (*) xs xs
-- fromList [1,4,9,16,25,36,49,64,81,100]

zipWithN :: (ZipWithN f b gr kr)
            => (a -> b -> gr) -> f a -> f b -> kr
zipWithN func xs = _zipWithN (fmap func xs)

----------------------------------------------------------------
----------------------------------------------------------- fin.
