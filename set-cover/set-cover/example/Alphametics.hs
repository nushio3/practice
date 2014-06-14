{- |
Solve the following verbal arithmetics puzzle:

>  SEND
> +MORE
> -----
> MONEY
-}
module Main where

import qualified Math.SetCover.Exact as ESC

import qualified Control.Monad.Trans.State as MS
import Control.Monad (liftM2, guard)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.HT as ListHT
import Data.Maybe (catMaybes)
import Data.Map (Map)
import Data.Set (Set)


data
   X =
      Variable Char | Digit Int |
      Column Int | Position Int Char Int | Carry Int Bool
         deriving (Eq, Ord, Show)

type Assign = ESC.Assign (Maybe (Char, Int)) (Set X)


assign :: (Ord b) => a -> [b] -> ESC.Assign a (Set b)
assign x ys = ESC.assign x $ Set.fromList ys

noAssign :: (Ord b) => [b] -> ESC.Assign (Maybe a) (Set b)
noAssign = assign Nothing

revIndex :: String -> Map Char Int
revIndex = Map.fromList . flip zip [0..] . reverse

histogram :: [String] -> Map Char [Int]
histogram =
   Map.unionsWith (++) . map (fmap (:[]) . revIndex)

variableAssigns :: [String] -> [Assign]
variableAssigns rows = do
   let leadingVars = Set.fromList $ map head rows
   (var, poss) <- Map.toList $ histogram rows
   (digit, otherDigits) <-
      (if Set.member var leadingVars
         then tail
         else id) $
      ListHT.removeEach [0..9]
   return $ assign (Just (var, digit)) $
      [Variable var, Digit digit] ++
      liftM2
         (\pos otherDigit -> Position pos var otherDigit)
         poss otherDigits

unassignedDigits :: [Assign]
unassignedDigits = map (ESC.assign Nothing . Set.singleton . Digit) [0..9]

add :: Int -> Char -> Char -> Char -> [Assign]
add pos xChar yChar zChar = do
   x <- [0..9]
   y <- [0..9]
   let (carryOut,z) = divMod (x+y) 10
   return $ noAssign
      [Column pos,
       Position pos xChar x, Position pos yChar y, Position pos zChar z,
       Carry (pos+1) (0/=carryOut)]

addCarry :: Int -> Char -> Char -> Char -> [Assign]
addCarry pos xChar yChar zChar = do
   x <- [0..9]
   y <- [0..9]
   carryIn <- [0..1]
   let (carryOut,z) = divMod (x+y+carryIn) 10
   return $ noAssign
      [Column pos,
       Position pos xChar x, Position pos yChar y, Position pos zChar z,
       Carry pos (0==carryIn), Carry (pos+1) (0/=carryOut)]

endCarry :: Int -> Char -> [Assign]
endCarry pos zChar = do
   carryIn <- [0,1]
   return $ noAssign
      [Column pos, Position pos zChar carryIn, Carry pos (0==carryIn)]

endCarry1 :: Int -> Char -> Char -> [Assign]
endCarry1 pos yChar zChar = do
   y <- [0..9]
   carryIn <- [0,1]
   let (carryOut,z) = divMod (y+carryIn) 10
   guard $ carryOut == 0
   return $ noAssign
      [Column pos,
       Position pos yChar y, Position pos zChar z,
       Carry pos (0==carryIn)]

-- one solution
assignsMoney :: [Assign]
assignsMoney =
   endCarry 4         'm' ++
   addCarry 3 's' 'm' 'o' ++
   addCarry 2 'e' 'o' 'n' ++
   addCarry 1 'n' 'r' 'e' ++
   add      0 'd' 'e' 'y' ++
   variableAssigns ["send", "more", "money"] ++
   unassignedDigits

-- 7 solutions
assigns224 :: [Assign]
assigns224 =
   endCarry 3         'f' ++
   addCarry 2 't' 't' 'o' ++
   addCarry 1 'w' 'w' 'u' ++
   add      0 'o' 'o' 'r' ++
   variableAssigns ["two", "two", "four"] ++
   unassignedDigits

-- 1200 solutions
assigns145 :: [Assign]
assigns145 =
   endCarry1 3     'f' 'f' ++
   addCarry  2 'o' 'o' 'i' ++
   addCarry  1 'n' 'u' 'v' ++
   add       0 'e' 'r' 'e' ++
   variableAssigns ["one", "four", "five"] ++
   unassignedDigits

-- no solutions
assigns358 :: [Assign]
assigns358 =
   endCarry1 4     't' 'e' ++
   addCarry  3 'f' 'h' 'i' ++
   addCarry  2 'i' 'r' 'g' ++
   addCarry  1 'v' 'e' 'h' ++
   add       0 'e' 'e' 't' ++
   variableAssigns ["five", "three", "eight"] ++
   unassignedDigits

-- no solutions
assigns459 :: [Assign]
assigns459 =
   addCarry  3 'f' 'f' 'n' ++
   addCarry  2 'o' 'i' 'i' ++
   addCarry  1 'u' 'v' 'n' ++
   add       0 'r' 'e' 'e' ++
   variableAssigns ["four", "five", "nine"] ++
   unassignedDigits

-- 32 solutions
assigns448 :: [Assign]
assigns448 =
   endCarry  4         'e' ++
   addCarry  3 'f' 'f' 'i' ++
   addCarry  2 'o' 'o' 'g' ++
   addCarry  1 'u' 'u' 'h' ++
   add       0 'r' 'r' 't' ++
   variableAssigns ["four", "four", "eight"] ++
   unassignedDigits

mainSetCover :: IO ()
mainSetCover =
   mapM_ (print . catMaybes) $ ESC.partitions assignsMoney



choose :: MS.StateT [a] [] a
choose = MS.StateT ListHT.removeEach

infixl 9 .:

(.:) :: Num a => a -> a -> a
x.:y = 10*x+y

bruteForceMoney :: [[(Char, Int)]]
bruteForceMoney = flip MS.evalStateT [0..9] $ do
   s <- choose; guard (s/=0)
   m <- choose; guard (m/=0)
   e <- choose
   n <- choose
   d <- choose
   o <- choose
   r <- choose
   y <- choose
   guard $ s.:e.:n.:d + m.:o.:r.:e == m.:o.:n.:e.:y
   return $
      ('s', s) : ('e', e) : ('n', n) : ('d', d) :
      ('m', m) : ('o', o) : ('r', r) : ('y', y) :
      []

mainBruteForce :: IO ()
mainBruteForce = mapM_ print bruteForceMoney


main :: IO ()
main = mainSetCover
