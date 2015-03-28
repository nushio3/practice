{-# OPTIONS -Wall #-}
module BrinkALP
       where

import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Packed.Matrix
import Numeric.LinearAlgebra.Algorithms (invlndet)

--Parameter---------------
width :: Double
width = 1.46
--------------------------

--nu = 0.23456558453744
nu :: Double
nu = 1.0 / 2.0 / width / width

unitL :: Double
unitL = 1.0 / (sqrt nu)

-- |@GaussALP x y z@
data GaussALP = GaussALP
                {-# UNPACK #-} !Double
                {-# UNPACK #-} !Double
                {-# UNPACK #-} !Double
              deriving(Show)

type SlaterWF = V.Vector GaussALP

-- |@BMatrix = ((zmat,bmat,binv),bdet)
type BMatrix = ((Matrix Double, Matrix Double, Matrix Double), Double)

makeGaussALP :: Double -> Double -> Double -> GaussALP
makeGaussALP x y z = GaussALP (x / unitL) (y / unitL) (z / unitL)

fixCMPosition :: SlaterWF -> SlaterWF
fixCMPosition wf = V.map (\(GaussALP x y z)
                        -> (GaussALP (x - xg) (y - yg) (z - zg))) wf
  where
    nmax = fromIntegral $ V.length wf
    [xg, yg, zg] = map (/nmax)
                   $ V.foldl' (\acc (GaussALP x y z) ->
                                zipWith (+) acc [x,y,z]) [0.0, 0.0, 0.0] wf

makeTetraHedronO16 :: Double -> SlaterWF
makeTetraHedronO16 d = V.fromList [alp1,alp2,alp3,alp4]
  where
    d' = d / unitL
    alp1 = makeGaussALP (d'/2.0) (d'/2.0/(sqrt 3.0))
           (-d'*(sqrt (2.0/3.0))/4.0)
    alp2 = makeGaussALP (-d'/2.0) (d'/2.0/(sqrt 3.0))
           (-d'*(sqrt (2.0/3.0))/4.0)
    alp3 = makeGaussALP (0.0) (-d'/2.0/(sqrt 3.0)) (-d'*(sqrt (2.0/3.0))/4.0)
    alp4 = makeGaussALP (0.0) (0.0) (3.0*d'*(sqrt (2.0/3.0))/4.0)

getZij :: GaussALP -> GaussALP -> Double            
getZij (GaussALP xi yi zi) (GaussALP xj yj zj)
  = ((xi - xj)**2 + (yi - yj)**2 + (zi - zj)**2)

getDD_ijkl :: GaussALP -> GaussALP -> GaussALP -> GaussALP -> Double
getDD_ijkl (GaussALP xi yi zi) (GaussALP xj yj zj) (GaussALP xk yk zk)
  (GaussALP xl yl zl) = (x * x + y * y + z * z) / 4.0
  where
    x = xi - xj + xk - xl
    y = yi - yj + yk - yl
    z = zi - zj + zk - zl

calcOverlap :: GaussALP -> GaussALP -> Double
calcOverlap bra ket = exp $ calcZiZj bra ket

calcZiZj :: GaussALP -> GaussALP -> Double
calcZiZj (GaussALP x1 y1 z1) (GaussALP x2 y2 z2) = zizj
  where
    xbra = (x1*x1 + y1*y1 + z1*z1) / 2.0
    xket = (x2*x2 + y2*y2 + z2*z2) / 2.0
    zizj = x1*x2 + y1*y2 + z1*z2 - xbra - xket

calcZmat :: SlaterWF -> SlaterWF -> Matrix Double
calcZmat bra ket = buildMatrix nmax nmax zizjfunc
  where
    nmax = fromIntegral $ V.length bra
    zizjfunc (r, c)
      | r >= c = calcZiZj (bra ! r) (ket ! c)
      | otherwise = zizjfunc (c, r)

calcBmat :: Matrix Double -> Matrix Double
calcBmat zmat = mapMatrix (exp) zmat
    
calcBInvDet :: Matrix Double -> (Matrix Double, Double)
calcBInvDet  bmat = (binv, bdet)
  where
    (binv,(log_det,_)) = invlndet bmat
    bdet = exp (4.0 * log_det)

getBMatrix :: SlaterWF -> SlaterWF -> BMatrix
getBMatrix bra ket = ((zmat,bmat,binv),bdet)
  where
    zmat = calcZmat bra ket
    bmat = calcBmat zmat
    (binv, bdet) = calcBInvDet bmat

op_Parity :: GaussALP -> GaussALP
op_Parity (GaussALP x y z) = (GaussALP (-x) (-y) (-z))
