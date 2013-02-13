{-# LINE 1 "Applications.lhs" #-}
#line 1 "Applications.lhs"







  {-# OPTIONS -XRankNTypes -XDeriveFunctor -XDeriveTraversable -XDeriveFoldable  -XTypeOperators -XFlexibleInstances -XMultiParamTypeClasses -XUndecidableInstances -XNoMonomorphismRestriction -fwarn-incomplete-patterns #-}

  module Applications where

  import Control.Applicative
  import Data.Foldable hiding (concat, all, concatMap, foldl)
  import Data.Traversable hiding (mapM, sequence)
  import Control.Monad
  import Data.List 
  import Control.Monad.State
  import Control.Monad.Identity
  import Generic




















































  data PatternF a = Term String | Epsilon | Seq a a | Alt a a 
   deriving (Functor, Foldable, Traversable) 


















 
  nullF :: PatternF Bool -> Bool
  nullF (Term s)     = False
  nullF Epsilon      = True
  nullF (Seq g1 g2)  = g1 && g2
  nullF (Alt g1 g2)  = g1 || g2
 











  nullable = sfold nullF False




  badNullable = cfold nullF








  g2 = Hide (Mu (               
     \(~(a:_)) -> [Alt (Var a)  (In (Term "x"))]))
























  firstF :: PatternF (Bool, [String]) -> [String]
  firstF (Term s)              = [s]
  firstF Epsilon               = []
  firstF (Seq (b1,a1) (_,a2))  = if b1 then union a1 a2 else a1
  firstF (Alt (_,a1) (_,a2))   = union a1 a2




  nullFirstF :: PatternF (Bool, [String]) -> (Bool, [String])
  nullFirstF = compose (leftPart nullF) firstF
 
  compose f g x = (f x, g x)
 
  leftPart :: Functor f => (f a -> a) -> f (a,b) -> a
  leftPart alg = alg . fmap fst




  firstSet = sfold nullFirstF (False,[])



























  type MGraph f a = State (Int, [a], [f (Rec f a)])




  addNode x = do  (pos, inn, out) <- get
                  put (pos+1, inn, out ++ [x])
                  return $ Var (inn !! pos)





  normF ::  PatternF (Rec PatternF a) -> 
            MGraph PatternF a (Rec PatternF a)
  normF x@(Term s)  = return $ In x
  normF x@Epsilon   = return $ In x
  normF x           = addNode x



  instance Applicative (State s) where
    pure = return 
    f <*> x = ap f x












  transformM :: (Traversable t, Applicative m, Monad m) 
       => (t (Rec f a) -> m (Rec f a))
       -> (([a], [t (Rec t a)]) -> m b -> [f (Rec f a)])
       -> ([t (Rec f a)] -> m b)
       -> Rec t a -> m (Rec f a)
  transformM f m h = trans where
     trans (Var x)  = pure (Var x)
     trans (In s)   = traverse trans s >>= f
     trans (Mu g)   = pure $ Mu (\l -> m (l, g l) (scan (g l)))
     scan out       = traverse (traverse trans) out >>= h

  normalize :: Graph PatternF -> Graph PatternF
  normalize x = Hide (evalState (process x) (0,[],[])) where 
     process = transformM normF runIt addNodes . reveal





  normalize2 :: Graph PatternF -> Graph PatternF
  normalize2 x = Hide (evalState (trans (reveal x)) (0,[],[])) 
 
  trans (Var x) = pure (Var x)
  trans (Mu g)  = pure $ Mu (\l -> runIt (l, g l) (scan (g l)))
  trans (In s)  = traverse trans s >>= normF
  scan o        = traverse (traverse trans) o >>= addNodes




  runIt (l, out) m = evalState m (length out,l,[]) 
  addNodes new = do 
     (_, _, nodes) <- get
     return (new ++ nodes)
















  instance Applicative Identity where



  atrans g = Hide (atrans' (reveal g)) 
  atrans' = runIdentity . 
      transformM (return . In . trans) 
                  (\_ -> runIdentity) 
                  (return . map trans) 
    where trans (Term s) = Term s
          trans Epsilon = Epsilon
          trans (Alt a b) = Seq a b
          trans (Seq a b) = Alt a b 

  instance MonadState s m => MonadState s (WrappedMonad m) where
    put x = WrapMonad (put x)
    get = WrapMonad get


  instance ShowF PatternF where
    showF sh Epsilon = "$"
    showF sh (Term s)   = "`" ++ s ++ "'"
    showF sh (Seq a b) = sh a ++ " " ++ sh b
    showF sh (Alt a b) = sh a ++ " | " ++ sh b

  g1 = Hide (Mu (
     \(~(a:b:c:d:_)) -> 
         [Alt (In (Seq (Var b) (Var c))) (Var d),  -- Rule 1
  --         Alt (In (Seq (In (Term "a")) (Var b))) (In (Term "x")),  -- Rule 2
          Alt (In (Seq (In (Term "a")) (Var b))) (In Epsilon),  -- Rule 2
          Alt (In (Seq (In (Term "c")) (Var d))) (Var b),                       -- Rule 3
          Seq (In (Term "b")) (Var a)                      -- Rule 4
         ]))

  main = do
     putStr $ showGraph g1
     print $ nullable g1
     print $ firstSet g1
     print $ firstSet g2
     putStr $ showGraph (normalize2 g1)
     putStr $ showGraph (atrans g1)


