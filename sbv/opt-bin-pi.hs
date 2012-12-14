{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Data.SBV

type Figure = SReal -> SReal -> SBool

circle :: Figure
circle x y = x^2 + y^2 .<= 1

data Rectangle = Rectangle
  { lef :: Rational, bot :: Rational, rig :: Rational, top :: Rational }

isAllIn, isAllOut :: Rectangle -> Figure -> Predicate

isAllIn Rectangle{..} fig =
  forAll ["x","y"] $ \x y ->
    (fromRational lef .<= x &&& x .<= fromRational rig &&&
     fromRational bot .<= y &&& y .<= fromRational top)
       ==> fig x y

isAllOut Rectangle{..} fig =
  forAll ["x","y"] $ \x y ->
    (fromRational lef .<= x &&& x .<= fromRational rig &&&
     fromRational bot .<= y &&& y .<= fromRational top)
       ==> bnot (fig x y)



main = do
  (print =<<) $ prove $ Rectangle 0 0 0.5 0.5 `isAllIn` circle
  (print =<<) $ prove $ Rectangle 1 1 2 2 `isAllOut` circle
  (print =<<) $ prove $ Rectangle 0 0 1 1 `isAllOut` circle
  (print =<<) $ prove $ Rectangle 0 0 1 1 `isAllIn` circle
