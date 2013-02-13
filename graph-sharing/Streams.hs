{-# LINE 1 "Streams.lhs" #-}
#line 1 "Streams.lhs"







  {-# OPTIONS -XRankNTypes -XNoMonomorphismRestriction -XScopedTypeVariables #-}

  module Streams where








  data PStream a v = 
       Var v 
    |  Mu (v -> PStream a v) 
    |  Cons a (PStream a v)
 
  newtype Stream a = HideStream {revealStream :: forall v . PStream a v}










  s1 = HideStream (Cons 1 (Mu (\{-"\rnode{A}{"-} v {-"}"-} -> Cons 2 (Var {-"\rnode{B}{"-} v {-"}"-})))) 



  s2 = HideStream (Mu (\{-"\rnode{C}{"-} v {-"}"-} -> Cons 1 (Cons 2 (Var {-"\rnode{D}{"-} v {-"}"-}))))



































  fix f = f (fix f)



  elems :: Stream a -> [a]
  elems = pelems . revealStream where -- a fold
    pelems :: PStream a [a] -> [a]
    pelems (Var v)      = v
    pelems (Mu g)       = pelems (g [])
    pelems (Cons x xs)  = x : pelems xs
















  foldStream :: (a -> b -> b) -> b -> Stream a -> b
  foldStream f k = pfoldStream . revealStream where
    pfoldStream (Var x)       = x
    pfoldStream (Mu g)        = pfoldStream (g k)
    pfoldStream (Cons x xs)   = f x (pfoldStream xs)



  foldStream2 :: (a -> b -> b) -> b -> Stream a -> b
  foldStream2 f k = pfoldStream . revealStream where
    pfoldStream (Var x)       = k
    pfoldStream (Mu g)        = pfoldStream (g ())
    pfoldStream (Cons x xs)   = f x (pfoldStream xs)





  elems' = foldStream (:) []









  cfoldStream :: (a -> b -> b) -> Stream a -> b
  cfoldStream f = pcfoldStream . revealStream where
    pcfoldStream (Var x)      = x 
    pcfoldStream (Mu g)       = fix (pcfoldStream . g)
    pcfoldStream (Cons x xs)  = f x (pcfoldStream xs)












  toList'  = cfoldStream (:)   
  upp2     = cfoldStream (\x s -> show x ++ " : " ++ s)




  toList :: Stream a -> [a]
  toList = ptoList . revealStream where -- a cyclic fold!
    ptoList (Var v)      = v
    ptoList (Mu g)       = fix (ptoList . g)
    ptoList (Cons x xs)  = x : ptoList xs 

  data S a = C a (S a)

  interpret :: Stream a -> S a
  interpret = sinterpret . revealStream where
    sinterpret (Var v)      = v
    sinterpret (Mu g)       = fix (sinterpret . g)
    sinterpret (Cons x xs)  = C x (sinterpret xs)

  pp :: Show a => Stream a -> String
  pp = ppp . revealStream where
     ppp (Var c)      = [c]
     ppp (Mu f)       = "Mu (\\a -> " ++ (ppp (f 'a')) ++ ")"
     ppp (Cons x xs)  = show x ++ " : " ++ ppp xs







  smap :: (a -> b) -> Stream a -> Stream b
  smap f s = HideStream (psmap f (revealStream s)) where
    psmap f (Var v)      = Var v
    psmap f (Mu g)       = Mu (psmap f . g)
    psmap f (Cons x xs)  = Cons (f x) (psmap f xs) 














  instance Eq a => Eq (Stream a) where
   s1 == s2 = peq 0 (revealStream s1) (revealStream s2) 
 
  peq ::  Eq a => 
          Int -> PStream a Int -> PStream a Int -> Bool
  peq n  (Var x)      (Var y)      = x == y
  peq n  (Mu f)       (Mu g)       = peq (n+1) (f n) (g n)
  peq n  (Cons x xs)  (Cons y ys)  = x == y && peq n xs ys
  peq _  _            _            = False
































  retPStream :: v -> PStream a v
  retPStream = Var



  joinPStream :: PStream a (PStream a v) -> PStream a v
  joinPStream (Var v)      = v
  joinPStream (Mu g)       = Mu (joinPStream . g . Var)
  joinPStream (Cons x xs)  = Cons x (joinPStream xs)


















  unrollStream :: Stream a -> Stream a
  unrollStream s = HideStream (joinPStream (punroll (revealStream s)))
  
  punroll ::  PStream a (PStream a v) -> 
              PStream a (PStream a v)
  punroll (Mu g)       = g (joinPStream (Mu g))
  punroll (Cons x xs)  = Cons x (punroll xs)


















  substPStream :: PStream a v -> (v -> PStream a v) -> PStream a v
  substPStream (Var v)      f  = f v
  substPStream (Mu g)       f  = Mu (\x -> substPStream (g x) f)
  substPStream (Cons x xs)  f  = Cons x (substPStream xs f)

  expandStream :: Stream a -> Stream a
  expandStream s = HideStream (unfoldPStream (revealStream s)) where
    unfoldPStream (Mu g)       = Mu (\x -> substPStream (g x) (const (g x)))
    unfoldPStream (Cons x xs)  = Cons x (unfoldPStream xs)

  -- snoc :: a -> Stream a -> Stream a
  -- snoc x s = HideStream (psnoc (x, revealStream s)) 

  headS :: Stream a -> a
  headS = pheadS . revealStream

  pheadS (Cons x xs) = x
  pheadS (Mu g) = pheadS (g undefined) 


  {-
  stail :: Stream a -> Stream a
  stail s = HideStream (ptail (revealStream s))
  
  ptail :: PStream a v -> PStream a v
  ptail (Mu g)       = Mu (\x -> 
    psnoc (cross (phead x, ptail) (g x)))
  ptail (Cons x xs)  = xs 
 
  psnoc :: (a, PStream a v) -> PStream a v
  psnoc (x, Var v)      = Cons x (Var v)
  psnoc (x, Mu g)       = Mu (curry psnoc x . g)
  psnoc (x, Cons y ys)  = Cons y (psnoc (x,ys)) 
 
  cross :: (a -> b, a -> c) -> a -> (b,c)
  cross (f,g) x = (f x, g x)
  -}





  stail :: Stream a -> Stream a
  stail s = HideStream (joinPStream (ptail (revealStream s))) where
    ptail (Cons x xs)  = xs
    ptail (Mu g)       = Mu (\x -> 
      let  phead (Mu g)       = phead (g x)
           phead (Cons y ys)  = y
      in ptail (g (Cons (phead (g x)) x)))



































  upp :: Show a => Stream a -> String
  upp = pupp . revealStream where
     pupp (Var v)      = v
     pupp (Mu g)       = fix (pupp . g)
     pupp (Cons x xs)  = show x ++ " : " ++ pupp xs


  instance Show a => Show (Stream a) where
    show = pp














  bisimilar :: Eq a => Stream a -> Stream a -> Bool
  bisimilar s1 s2 = pbisimilar [] 0 (revealStream s1) (revealStream s2) where
    pbisimilar :: Eq a => [(Int, PStream a Int)] -> Int -> PStream a Int -> PStream a Int -> Bool
    pbisimilar env n (Var x) (Var y)         = x == y
    pbisimilar env n (Cons x xs) (Cons y ys) = x == y && pbisimilar env (n+1) xs ys
    pbisimilar env n (Mu f) p                = pbisimilar ((n, Mu f):env) n (f n) p
    pbisimilar env n p (Mu g)                = pbisimilar ((n, Mu g):env) n p (g n)   

  func :: Stream a -> PStream a (Stream a)
  func = revealStream

  ifunc :: PStream a (Stream a) -> Stream a
  ifunc (Var s) = s
  ifunc (Mu g) = HideStream (Mu (\x -> undefined ))
  ifunc (Cons x xs) = HideStream (Cons x (revealStream (ifunc xs)))


























































  {-
  instance Eq a => Eq (Stream a) where
   s1 == s2 = peq (revealStream s1) (revealStream s2) where
     peq :: Eq a => PStream () a -> PStream () a -> Bool
     peq (Var _) (Var _)          = True
     peq (Mu f) (Mu g)            = peq (f ()) (g ())
     peq (Cons x xs) (Cons y ys)  = x == y && peq xs ys
     peq _ _                      = False
  -}




  {-
  instance Eq a => Eq (Stream a) where
   s1 == s2 = peq (revealStream s1) (revealStream s2) where
     peq :: Eq a => PStream Int a -> PStream Int a -> Reader Int Bool
     peq (Var _) (Var _)          = True
     peq (Mu f) (Mu g)            = peq (f ()) (g ())
     peq (Cons x xs) (Cons y ys)  = x == y && peq xs ys
     peq _ _                      = False
  -}



  hd :: Stream a -> a
  hd = phd . revealStream where
     phd :: PStream a a -> a
     phd (Mu g)       = fix (phd . g)
     phd (Cons x xs)  = x





  {-
  tl :: Stream a -> Stream a
  tl s = HideStream (ptl (hd s) (revealStream s)) where
     ptl :: a -> PStream a v -> PStream a v
     ptl x (Var v)      = Var v
     ptl x (Mu g)       = Mu (ptl x . psnoc x . g)
     ptl x (Cons y ys)  = ys
  -}






