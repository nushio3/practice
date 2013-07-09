import Control.Monad
import Data.Packed.Matrix
import Data.Char
import Numeric.Container
import Numeric.LinearAlgebra.Algorithms
import Text.Printf

t :: Matrix Double

showMat :: Matrix Double -> String
showMat m = unlines $ 
  map (map (head . show)) $
  toLists m

t = fromLists $
    map (map $ (\x->x-48). fromIntegral . ord) 
      ["111", "011", "001"]
--     [ "10000000"
--     , "01000000"
--     , "00100000"
--     , "00010011"
--     , "00000000"
--     , "00001000"
--     , "00000100"
--     , "00000000"
--     ]

main :: IO ()
main = do
  putStrLn $ showMat $ t <> t <> t
  let (ev,em) =  eig t
  print $ ev
  print ""
  mapM_ print $ toRows $ em