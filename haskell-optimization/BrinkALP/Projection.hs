{-# OPTIONS -Wall #-}
module Projection
       where

import BrinkALP
import qualified Data.Vector as V
import Data.Array.Unboxed
import Data.List

-- |should be even number to aboid pi/2!
nalpmax,nbetmax,ngammax :: Int
nalpmax = 20
nbetmax = 32    
ngammax = 20
--
halp,hbet,hgam,hxyz:: Double
halp = 2.0 * pi / fromIntegral (nalpmax - 1)
hbet = pi / fromIntegral (nbetmax - 1)
hgam = 2.0 * pi / fromIntegral (ngammax - 1)
hxyz = halp * hbet * hgam

am_proj_0 :: (SlaterWF -> SlaterWF -> (Double, Double))
             -> (Double -> Double -> Double)  -- (+) or (-)
             -> SlaterWF -> SlaterWF -> (Double, Double)
am_proj_0 h p bra ket = (se * hxyz, sw * hxyz)
  where
    (se, sw) = foldl1'
               (\(ae, aw) (e, w) -> (ae + e, aw + w)) smat
    smat =
      [(e * sin_bet * wxyz ! (i,j,k), w * sin_bet * wxyz ! (i,j,k))
      | i <- [1..nalpmax], j <- [2..(nbetmax-1)], k <- [1..ngammax]
      , let alp = halp * fromIntegral (i - 1)
            bet = hbet * fromIntegral (j - 1)
            gam = hgam * fromIntegral (k - 1)
            sin_bet = sin bet
            rotmat = getRotMatrix alp bet gam
            ket' = V.map (op_RotMatrix rotmat) ket
            (e, w) = parity_proj h p bra ket']

-- |perform the parity projection. @parity_proj H p bra ket@. @p = (+)@ and @(-)@ for the positive and negative parity states, respectively.
parity_proj :: (SlaterWF -> SlaterWF -> (Double, Double))
               -> (Double -> Double -> Double)  -- (+) or (-)
               -> SlaterWF -> SlaterWF -> (Double, Double)
parity_proj h p bra ket = ((wd * hd) `p` (we * he), wd `p` we)
  where
    (hd, wd) = bra `h` ket
    (he, we) = bra `h` (V.map op_Parity ket)

op_RotMatrix :: UArray (Int,Int) Double -> GaussALP -> GaussALP
op_RotMatrix rmat (GaussALP x y z) = (GaussALP x' y' z')
  where
    x' = rmat ! (1,1) * x + rmat ! (1,2) * y + rmat ! (1,3) * z
    y' = rmat ! (2,1) * x + rmat ! (2,2) * y + rmat ! (2,3) * z
    z' = rmat ! (3,1) * x + rmat ! (3,2) * y + rmat ! (3,3) * z

getRotMatrix :: Double -> Double -> Double -> UArray (Int,Int) Double
getRotMatrix alp bet gam = listArray ((1,1),(3,3)) [rot11,rot12,rot13
                                                   ,rot21,rot22,rot23
                                                   ,rot31,rot32,rot33]
  where
    cos_alp = cos alp
    sin_alp = sin alp
    cos_bet = cos bet
    sin_bet = sin bet
    cos_gam = cos gam
    sin_gam = sin gam
    rot11 =  cos_alp * cos_bet * cos_gam - sin_alp * sin_gam
    rot12 = -cos_alp * cos_bet * sin_gam - sin_alp * cos_gam
    rot13 =  cos_alp * sin_bet
    rot21 =  sin_alp * cos_bet * cos_gam + cos_alp * sin_gam
    rot22 = -sin_alp * cos_bet * sin_gam + cos_alp * cos_gam
    rot23 =  sin_alp * sin_bet
    rot31 = -sin_bet * cos_gam
    rot32 =  sin_bet * sin_gam
    rot33 =  cos_bet

wxyz :: UArray (Int,Int,Int) Double
wxyz = listArray ((1,1,1),(nalpmax,nbetmax,ngammax))
       [ x*y*z | x <- 0.5 : (replicate (nalpmax - 2) 1.0) ++ [0.5]
               , y <- 0.5 : (replicate (nbetmax - 2) 1.0) ++ [0.5]
               , z <- 0.5 : (replicate (ngammax - 2) 1.0) ++ [0.5]]

