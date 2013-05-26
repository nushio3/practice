{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Data.SBV
import Text.Printf

type Figure = SReal -> SReal -> SBool

circle :: Figure
circle x y = x^2 + y^2 .<= 1

data Rectangle = Rectangle
  { lef :: Rational, bot :: Rational, rig :: Rational, top :: Rational }
  deriving (Eq, Show)

area :: Rectangle -> Rational
area Rectangle{..} = (rig-lef)*(top-bot)

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

quarters :: Rectangle -> [Rectangle]
quarters Rectangle{..} =
  [ Rectangle lef  bot  midx midy
  , Rectangle midx bot  rig  midy
  , Rectangle lef  midy midx top
  , Rectangle midx midy rig  top
  ]
  where
    midx = (lef+rig)/2
    midy = (bot+top)/2

main = do
  go circle 0 1 $ [Rectangle 0 0 1 1] >>= quarters

go :: Figure -> Rational -> Rational ->  [Rectangle] -> IO ()
go fig lower upper rects = do
  printf "%f <= pi <= %f\n"
    (4*fromRational lower::Double)
    (4*fromRational upper::Double)
  sols <- forM rects $ \rect -> do
    p1 <- isTheorem $ rect `isAllIn` fig
    p2 <- isTheorem $ rect `isAllOut` fig
    return (p1, p2, rect)
  let newLower = lower + sum [area r| (True, _, r) <- sols ]
      newUpper = upper - sum [area r| (_, True, r) <- sols ]
      newRects = [r| (False, False, r) <- sols ] >>= quarters
  go fig newLower newUpper newRects
