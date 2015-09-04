import Control.Monad.State


test :: State [Int] ()
test = do
  test
  modify (1:)

main :: IO ()
main = do
  print $ take 10 $ snd $ runState test []
