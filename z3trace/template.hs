import Data.SBV
import System.Environment

main :: IO ()
main = do
  argv <- getArgs
  if "-2" `elem` argv then
    print =<< proveWith z3{printBase=2} theorem
    else
    print =<< prove theorem

theorem :: Predicate
theorem = do
