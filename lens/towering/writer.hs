import Control.Lens
import Control.Lens.Setter
import Control.Monad.Writer
import Data.Monoid

prog :: WriterT (String,[Int]) IO ()
prog = do
  scribe (_1 :: Lens' (String, [Int]) String) "I" 
  tell (" ", [])
  scribe (_2 :: Lens' (String, [Int]) [Int]) [2, 3]
  tell ("love" , [4])
  
main :: IO ()
main = do
  ((), (ret,xs)) <- runWriterT prog
  putStrLn ret
  print $ (1::Int) : xs
