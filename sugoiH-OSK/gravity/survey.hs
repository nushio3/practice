import Control.Monad
import Data.List

w :: Int
w = 4

lim :: Int
lim = 2

streak :: [Bool] -> Int
streak = maximum . map length . group

guardStreak :: (MonadPlus m) => [Bool] -> m ()
guardStreak = sequence_ . map (\n -> guard (n <= lim)) . map length . group

mkLine = do
  let cands = replicate w [True, False]
  xs <- sequence cands
  guardStreak xs
  return xs

mkLines h = do
  xss <- replicateM h mkLine
  mapM_ guardStreak xss
  return xss

pp :: [[Bool]] -> IO ()
pp =
  mapM_ putStrLn .
  map (map (\b -> if b then 'O' else 'X'))

main = do
  pp $ head $ mkLines 4
