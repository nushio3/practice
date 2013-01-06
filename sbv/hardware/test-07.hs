import BinarySearch

import Data.SBV
import Text.Printf

main = do
  cost <- budgetSatProb
  putStrLn $ "budget = " ++ show(fromRational cost :: Double)

budgetSatProb :: IO Rational
budgetSatProb = binarySearchWith z3 defaultBSOpts p
  where
    p :: Rational -> Predicate
    p budgetReal = do
      let budget :: SReal
          budget = fromRational budgetReal
      num1 <- sInteger "num1"
      num2 <- sInteger "num2"

      let numR1 = toSReal num1
          numR2 = toSReal num2

      constrain $ num1 .>= 100
      constrain $ num2 .>= 100
      constrain $ budget .>= numR1 * numR2
      return (true :: SBool)
