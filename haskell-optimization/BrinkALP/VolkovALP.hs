{-# OPTIONS -Wall #-}
module VolkovALP
       where

import Data.List
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Packed.Matrix
import GlobalConst (hbarc, mnuc, electron2)
import BrinkALP

--Parameter---------------------------------
volkovP :: [(Double,Double)]
volkovP = [(-60.65,0.308642)
          ,( 61.14,0.980296)]
          
maj :: Double
maj = 0.62
--------------------------------------------

wig :: Double
wig = 1.0 - maj

gammaP :: [(Double,Double)]
gammaP = map (\(v0, mu)
              -> (v0 * (nu / (nu + mu))**(3.0/2.0), mu / (nu + mu)))
         volkovP

vkin0 :: Double
vkin0 = 0.5 * hbarc * hbarc * nu / mnuc

vc0 :: Double
vc0 = electron2 * (sqrt nu) * 2.0 / (sqrt pi)

hVolkov' :: BMatrix -> SlaterWF -> SlaterWF -> (Double, (Double, Double))
hVolkov' ((zmat,bmat,binv),_) bra ket = (vkin + vpot, (vkin, vpot))
  where
    vkin = calcVkin bra ket bmat binv
    vpot = calcVpot bra ket zmat binv

hVolkov :: SlaterWF -> SlaterWF -> (Double, Double)
hVolkov bra ket = (vkin + vpot, bdet)
  where
    ((zmat,bmat,binv),bdet) = getBMatrix bra ket
    vkin = calcVkin bra ket bmat binv
    vpot = calcVpot bra ket zmat binv

calcVkin :: SlaterWF -> SlaterWF -> Matrix Double -> Matrix Double -> Double
calcVkin bra ket bmat binv = vkin1 + vkin2
  where
    nmax = (V.length bra) - 1
    vkin1 =  3.0 * vkin0 * (fromIntegral ((nmax + 1) * 4 - 1))
    vkin2 = -4.0 * vkin0
            * foldl1' (+) [getZij (bra ! i) (ket ! j)
                           * (bmat @@> (i, j)) * (binv @@> (j, i))
                          |i <- [0..nmax], j <- [0..nmax]]

calcVpot :: SlaterWF -> SlaterWF -> Matrix Double -> Matrix Double -> Double
calcVpot bra ket zmat binv = vdiag / 2.0 + voff
  where
    nmax = (V.length bra) - 1
    vdiag = foldl1' (+) [vmat bra ket zmat binv i i |i <- [0..nmax]]
    voff  = foldl1' (+) [vmat bra ket zmat binv i j
                        |i <- [0..nmax], j <- [(i+1)..nmax]]

vmat :: SlaterWF -> SlaterWF -> Matrix Double -> Matrix Double
        -> Int -> Int -> Double
vmat bra ket zmat binv i j
  = foldl1' (+)
    [let rd = getDD_ijkl (bra ! i) (bra ! j) (ket ! k) (ket ! l)
         rd0 = (zmat @@> (i, k)) + (zmat @@> (j, l))
         re = getDD_ijkl (bra ! i) (bra ! j) (ket ! l) (ket ! k)
         re0 = (zmat @@> (i, l)) + (zmat @@> (j, k))
         rpair = [(rd, rd0), (re, re0)]
         [ud,ue] = [ foldl1' (+)
                     $ map (\(v0, gam) -> v0 * exp (-gam * r + r0)) gammaP
                   | (r, r0) <- rpair]
         [ucd,uce] = [ vc0 * (getCoulombInt r) * exp r0
                     |(r,r0) <- rpair]
         vd = ud * (16.0 * wig -  4.0 * maj) + 4.0 * ucd
         ve = ue * (-4.0 * wig + 16.0 * maj) - 2.0 * uce
     in (vd + ve) * (binv @@> (k, i)) * (binv @@> (l, j))
    |k <- [0..nmax], l <- [0..nmax]]
  where
    nmax = (V.length bra) - 1

getCoulombInt :: Double -> Double
getCoulombInt dd = calcGaussLegendre (\x -> exp (-dd * x*x))

calcGaussLegendre :: (Double -> Double) -> Double
calcGaussLegendre f
  = 0.5 * foldl1' (+) [ wl * (f cp + f cm)
                      | (cp, cm, wl) <- getGaussLegendre8']
    
getGaussLegendre8' :: [(Double,Double,Double)]
getGaussLegendre8'
  =[(0.591717321247825,0.4082826787521751,0.3626837833783622)
   ,(0.7627662049581645,0.23723379504183556,0.3137066458778873)
   ,(0.8983332387068134,0.10166676129318664,0.2223810344533746)
   ,(0.980144928248768,1.9855071751231967e-2,0.1012285362903763)]
   
