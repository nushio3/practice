{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
import Control.Lens

data TreeF x = Branch x x

class HasTreeF a where
  treeF :: Lens' (a x) (TreeF x)

instance HasTreeF TreeF where
  treeF = lens id const

data Prod fa fb x = Prod (fa x) (fb x)
data Sum fa fb x = InjL (fa x) | InjR (fb x)


instance HasTreeF fa => HasTreeF (Prod fa fb) where
  treeF = let l f (Prod a1 b) = fmap (\a2 -> Prod a2 b) (f a1)
          in l . treeF


{-
instance HasTreeF fb => HasTreeF (Prod fa fb) where
  treeF = let l f (Prod a b1) = fmap (\b2 -> Prod a b2) (f b1)
          in l . treeF
-}
main :: IO ()
main = print "hi"


{-
MRMに不足している点
関数をfoldで書かなければならないため、二段階以上のパターンマッチングができない。

Subclassing is awesome.

Product に対応していない。
-}
