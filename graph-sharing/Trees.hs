{-# LINE 1 "Trees.lhs" #-}
#line 1 "Trees.lhs"











  {-# OPTIONS -XRankNTypes #-}

  module Trees where

  import Control.Monad
  import Control.Monad.Reader hiding (fix)
  import Control.Monad.State hiding (fix)

  fix f = f (fix f)





  data PTree' v a = Var' v | Mu' (v -> PTree' v a) | Empty' | Fork' a (PTree' v a) (PTree' v a)

  newtype Tree' a = HideTree' {revealTree' :: forall v . PTree' v a}

  t1' = HideTree' (Mu' (\x -> Fork' 1 (Var' x) (Fork' 2 (Var' x) Empty')))













  foldTree :: (a -> b -> b -> b) -> b -> b -> Tree a -> b
  foldTree f k1 k2 s = trans (revealTree s) where
    trans (Var x)        = x
    trans (Mu g)         = head (map trans (g (repeat k1))) 
    trans Empty          = k2
    trans (Fork x l r)   = f x (trans l) (trans r)



  cfoldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
  cfoldTree f k s = trans (revealTree s) where
    trans (Var x)      = x
    trans (Mu g)       = head (fix (map trans . g))
    trans Empty        = k
    trans (Fork x l r) = f x (trans l) (trans r)



  tmap :: (a -> b) -> Tree a -> Tree b
  tmap f s = HideTree (ptmap f (revealTree s)) where
    ptmap f (Var x)       = Var x
    ptmap f (Mu g)        = Mu (map (ptmap f) . g)
    ptmap f Empty         = Empty
    ptmap f (Fork x l r)  = Fork (f x) (ptmap f l) (ptmap f r)













  instance Eq a => Eq (Tree a) where
    t1 == t2 = peq 0 (revealTree t1) (revealTree t2)
 
  peq :: Eq a => Int -> PTree a Int -> PTree a Int -> Bool
  peq _  (Var x)         (Var y)          = x == y
  peq n  (Mu f)          (Mu g)           = 
    let  l1 = f (iterate succ n)
         l2 = g (iterate succ n)
    in  and $ zipWith (peq (n + length l1)) l1 l2
  peq n Empty            Empty            = True
  peq n (Fork x1 l1 r1)  (Fork x2 l2 r2)  = 
    x1 == x2 && peq n l1 l2 && peq n r1 r2
  peq _  _               _                = False





  joinPTree :: PTree a (PTree a v) -> PTree a v
  joinPTree (Var v)       = v
  joinPTree (Mu g)        = Mu (map joinPTree . g . map Var)
  joinPTree Empty         = Empty
  joinPTree (Fork x l r)  = Fork x (joinPTree l) (joinPTree r)





  unrollTree :: Tree a -> Tree a
  unrollTree s = HideTree (joinPTree (unrollPTree (revealTree s)))
 
  unrollPTree :: PTree a (PTree a v) -> PTree a (PTree a v)
  unrollPTree (Mu g)        = head (g (repeat (joinPTree (Mu g))))
  unrollPTree Empty         = Empty
  unrollPTree (Fork x l r)  = Fork x (unrollPTree l) (unrollPTree r)












  data PTree a v = 
      Var v 
   |  Mu ([v] -> [PTree a v]) 
   |  Empty 
   |  Fork a (PTree a v) (PTree a v) 
 
  newtype Tree a = HideTree {revealTree :: forall v . PTree a v}






  t1 = HideTree (Mu (\(~(x:_)) -> 
     [Fork 1 (Fork 2 (Var x) Empty) (Var x)]))






  t2 = HideTree (Mu (\(~(x:y:_)) -> 
     [Fork 1 (Var y) (Var x), Fork 2 (Var x) (Var y)]))


















































  t3 = HideTree (Mu (\_ -> [Fork 1 Empty Empty, Fork 2 Empty Empty]))



  cfoldTree2 :: ([b] -> b) -> (a -> b -> b -> b) -> b -> Tree a -> b
  cfoldTree2 h f k = pcfoldTree . revealTree where
    pcfoldTree (Var x)        = x
    pcfoldTree (Mu g)         = h (fix (map pcfoldTree . g))
    pcfoldTree Empty          = k
    pcfoldTree (Fork x l r)   = f x (pcfoldTree l) (pcfoldTree r)

  cfoldTree3 :: (([b] -> [b]) -> b) -> (a -> b -> b -> b) -> b -> Tree a -> b
  cfoldTree3 h f k = pcfoldTree . revealTree where
    pcfoldTree (Var x)        = x
    pcfoldTree (Mu g)         = h (map pcfoldTree . g) 
    pcfoldTree Empty          = k
    pcfoldTree (Fork x l r)   = f x (pcfoldTree l) (pcfoldTree r)

  cfoldTree4 h f k s = cfoldTree3 (\g -> h (g s)) f k
  cfoldTree5 h = cfoldTree3 (h . fix)



  mfoldTree :: Monad m => (forall a . m a -> a) -> (a -> b -> b -> m b) -> m b -> Tree a -> m b
  mfoldTree runM f k = pmfoldTree . revealTree where
    pmfoldTree (Var x)       = return x
    pmfoldTree (Mu g)        = return (head (fix (runM . mapM pmfoldTree . g)))
    pmfoldTree Empty         = k
    pmfoldTree (Fork x l r)  = do  l' <- pmfoldTree l
                                   r' <- pmfoldTree r
                                   f x l' r'

  flipbind f x = x >>= f



  mfoldTree2 :: Monad m => ([b] -> m b) -> (a -> b -> b -> m b) -> m b -> Tree a -> m b
  mfoldTree2 h f k = pmfoldTree . revealTree where
    pmfoldTree (Var x)       = return x
    pmfoldTree (Mu g)        = fix (flipbind (mapM pmfoldTree . g)) >>= h
    pmfoldTree Empty         = k
    pmfoldTree (Fork x l r)  = do  l' <- pmfoldTree l
                                   r' <- pmfoldTree r
                                   f x l' r'



  ppIndent t = evalState (mfoldTree2 (return . show) (\x l r -> do {s <- get; put x; return (show s ++ ppFork x l r ++ "\n")}) (return ppEmpty) t) 1 



  {-
  pp t = evalState (mfoldTree2 showL showFork showEmpty t) (iterate succ 'a') where
     showEmpty = return "Empty"
     showFork x l r = return ("Fork " ++ show x ++ " (" ++ show l ++ ") (" ++ show r ++ ")")
     showL r = 
       do  seed <- get
           let (fresh, seed') = splitAt (length r) seed
           put seed'
           return ("Mu (\n" ++ concat [ "  " ++ [a] ++ " => " ++ v ++ "\n" | (a,v) <- zip fresh r] ++ ")\n") 
  -}



  uppTree :: Show a => Tree a -> String
  uppTree = cfoldTree2 show ppFork ppEmpty 

  ppFork x l r = "Fork " ++ show x ++ " (" ++ l ++ ") " ++ " (" ++ r ++ ") " 
  ppEmpty      = "Empty"


  substPTree :: PTree a v -> (v -> PTree a v) -> PTree a v
  substPTree (Var v)       f  = f v
  substPTree (Mu g)        f  = Mu (\x -> map (flip substPTree f) (g x))
  substPTree Empty         f  = Empty 
  substPTree (Fork x l r)  f  = Fork x (substPTree l f) (substPTree r f)

  expandTree :: Tree a -> Tree a
  expandTree s = HideTree (unfoldPTree (revealTree s)) where
    unfoldPTree :: PTree a v -> PTree a v
    unfoldPTree (Mu g)         = Mu (\x -> map (\t -> substPTree t (const t)) (g x))
    unfoldPTree Empty          = Empty
    unfoldPTree (Fork x l r)   = Fork x (unfoldPTree l) (unfoldPTree r)


  {-
  ppTree :: Show a => Tree a -> String
  ppTree t = cfoldTree3 ppList ppFork ppEmpty t (iterate succ 'a') where
    ppEmpty _          = "Empty"
    ppFork x l r seed  = "Fork " ++ show x ++ " (" ++ l seed ++ ") " ++ " (" ++ r seed ++ ") "
    ppList :: (([[Char] -> String] -> [[Char] -> String]) -> [Char] -> String) -> [Char] -> String 
    ppList h seed      =  
      "Mu (\n" ++ concat [ "  " ++ [a] ++ " => " ++ v ++ "\n" | (v,a) <- zipWith (\c f -> (f seed',c)) fresh r] ++ ")\n" where
         r              = h fix seed
         (fresh, seed') = splitAt (length r) seed
  -}



















































































































































































