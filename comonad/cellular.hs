-- http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html




class Additive a where
    zero     :: a
    (+), (-) :: a -> a -> a
    negate   :: a -> a

{- |
          a + b === b + a
    (a + b) + c === a + (b + c)
       zero + a === a
   a + negate a === 0
-}

class (Additive a) => Ring a where
    (*)         :: a -> a -> a
    one         :: a

{- |
  a * (b * c) === (a * b) * c
      one * a === a
      a * one === a
  a * (b + c) === a * b + a * c
-}


import Control.Comonad

data U x = U [x] x [x]

right (U ls x (r:rs)) = U (x:ls) r rs
left  (U (l:ls) x rs) = U ls l (x:rs)

instance Functor U where
  fmap f (U ls x rs) = U (map f ls) (f x) (map f rs)

instance Comonad U where
  extract (U _ x _) = x
  duplicate a = U (tail $ iterate left a) a (tail $ iterate right a)



rule :: U Bool -> Bool
rule (U (a:_) b (c:_)) = not (a && b && not c || (a==b))


shift i u = (iterate (if i<0 then left else right) u) !! abs i

toList i j u = take (j-i) $ half $ shift i u where
   half (U _ b c) = [b] ++ c

main = let u = U (repeat False) True (repeat False)
           n = 50
      in putStr $
         unlines $
         take n $
         map (map (\x -> if x then '#' else ' ') . toList (-n) n) $
         iterate (extend rule) u
