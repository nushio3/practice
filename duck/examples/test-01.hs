import Control.Lens
import Data.DuckTyped
import Data.DuckTyped.Flying

x1,x2 :: Object
x1 = empty

x2 = x1 & speed .~ 120


main = do
  print x1
  print $ x1 ^. speed
  print x2
  print $ x2 ^. speed