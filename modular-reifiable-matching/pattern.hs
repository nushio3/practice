{-# LANGUAGE PatternSynonyms #-}

import Control.Lens

pattern List :: Double -> [Double]
pattern List a = [a]

-- match方向にはマッチに失敗しても良いが、
-- コンストラクタの失敗は許されない。

main :: IO ()
main = do
  print $ List 32
  let x = [42]
      List y = x
  print $ x
