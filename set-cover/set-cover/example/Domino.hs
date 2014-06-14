{-
Juergen Goering
Labyrinth der Denkspiele, Seite 127

Zerlege folgende Felder in Dominosteine:

3134205
3110266
3550426
6321201
5045254
3660301
5451243
6264410


25114225
25304365
11305361
24465661
23560204
63540204
60043311
-}
module Main where

import qualified Math.SetCover.Exact as ESC

import qualified Data.Char.Frame as Frame
import qualified Data.List.HT as ListHT
import qualified Data.Set as Set
import Control.Applicative (pure)
import Data.Set (Set)
import Data.Monoid (Monoid, mempty, mappend, mconcat)


data
   Score = S0 | S1 | S2 | S3 | S4 | S5 | S6
      deriving (Eq, Ord, Show, Enum, Bounded)

data
   X = Brick Score Score | Position Int Int
         deriving (Eq, Ord, Show)

type Position = (Int, Int)

data
   Borders = Borders {
      vertical, horizontal :: Set Position
   }

type Assign = ESC.Assign Borders (Set X)

instance Monoid Borders where
   mempty = Borders mempty mempty
   mappend x y =
      Borders {
         vertical = Set.union (vertical x) (vertical y),
         horizontal = Set.union (horizontal x) (horizontal y)
      }


above, left :: Position -> Position
above (row,col) = (row-1, col)
left  (row,col) = (row, col-1)

below, right :: Position -> Position
below (row,col) = (row+1, col)
right (row,col) = (row, col+1)


brick :: Score -> Score -> X
brick x y = Brick (min x y) (max x y)

assign :: (Ord a) => map -> [a] -> ESC.Assign map (Set a)
assign m xs = ESC.assign m (Set.fromList xs)

attachPositions :: [[a]] -> [[(Position, a)]]
attachPositions = zipWith (\x -> zipWith (\y -> (,) (x,y)) [0..]) [0..]

assigns :: [[Score]] -> [Assign]
assigns xs =
   let ps = attachPositions xs
   in  concat
          (ListHT.mapAdjacent
             (zipWith
                 (\(p0,b0) (p1,b1) ->
                    assign
                       (Borders {
                           vertical =
                              Set.fromList $
                              p0 : p1 : right p0 : right p1 : [],
                           horizontal = Set.fromList $ p0 : below p1 : []
                        })
                       [brick b0 b1, uncurry Position p0, uncurry Position p1]))
             ps)
       ++
       concatMap
          (ListHT.mapAdjacent
              (\(p0,b0) (p1,b1) ->
                 assign
                    (Borders {
                        horizontal =
                           Set.fromList $
                           p0 : p1 : below p0 : below p1 : [],
                        vertical = Set.fromList $ p0 : right p1 : []
                     })
                    [brick b0 b1, uncurry Position p0, uncurry Position p1]))
          ps


formatCorner, formatHorizontal, formatVertical :: Borders -> Position -> Char
formatCorner m p =
   Frame.simple $
   Frame.Parts
      (fmap (flip Set.member (vertical   m)) $ Frame.Vertical   (above p) p)
      (fmap (flip Set.member (horizontal m)) $ Frame.Horizontal (left  p) p)
formatHorizontal m p =
   Frame.simple (Frame.Parts (pure False) (pure $ Set.member p (horizontal m)))
formatVertical m p =
   Frame.simple (Frame.Parts (pure $ Set.member p (vertical m)) (pure False))

{- |
@mapIntersperse f g [a,b,c]@
computes
@[f 0, g 0 a, f 1, g 1 b, f 2, g 2 c, f 3]@
-}
mapIntersperse :: (Int -> b) -> (Int -> a -> b) -> [a] -> [b]
mapIntersperse f g xs =
   f 0 : concat (zipWith (\n x -> [g n x, f (n+1)]) [0..] xs)

format :: [[Score]] -> Borders -> String
format xss m =
   unlines $
   mapIntersperse
      (\row ->
         mapIntersperse
            (\col -> formatCorner m (row,col))
            (\col _ -> formatHorizontal m (row,col))
            (case xss of xs:_ -> xs; [] -> []))
      (\row ->
         mapIntersperse
            (\col -> formatVertical m (row,col))
            (\ _col n -> toEnum $ fromEnum '0' + fromEnum n))
      xss


fieldSimple :: [[Score]]
fieldSimple = map (map toEnum) $
   (3:1:3:4:2:0:5:[]) :
   (3:1:1:0:2:6:6:[]) :
   (3:5:5:0:4:2:6:[]) :
   (6:3:2:1:2:0:1:[]) :
   (5:0:4:5:2:5:4:[]) :
   (3:6:6:0:3:0:1:[]) :
   (5:4:5:1:2:4:3:[]) :
   (6:2:6:4:4:1:0:[]) :
   []

fieldRussian :: [[Score]]
fieldRussian = map (map toEnum) $
   (2:5:1:1:4:2:2:5:[]) :
   (2:5:3:0:4:3:6:5:[]) :
   (1:1:3:0:5:3:6:1:[]) :
   (2:4:4:6:5:6:6:1:[]) :
   (2:3:5:6:0:2:0:4:[]) :
   (6:3:5:4:0:2:0:4:[]) :
   (6:0:0:4:3:3:1:1:[]) :
   []

main :: IO ()
main =
   let field = fieldRussian
   in  mapM_ (putStrLn . format field . mconcat) $
          ESC.partitions $ assigns field
