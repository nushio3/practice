{-# LINE 1 "RecursiveBinders.lhs" #-}
#line 1 "RecursiveBinders.lhs"







  {-# OPTIONS -XRankNTypes #-}

  module RecursiveBinders where

  import PHoas

  import Control.Monad.Reader hiding (fix)
















  data PLambda2 a =  PFix2 (a -> PLambda2 a) | {-"\ldots"-}



      PVar2 a 
   |  PLit2 Int
   |  PBool2 Bool
   |  PIf2 (PLambda2 a) (PLambda2 a) (PLambda2 a)
   |  PMult2 (PLambda2 a) (PLambda2 a)
   |  PEq2 (PLambda2 a) (PLambda2 a) 
   |  PAdd2 (PLambda2 a) (PLambda2 a) 
   |  PLam2 (a -> PLambda2 a) 
   |  PApp2 (PLambda2 a) (PLambda2 a)

  newtype Lambda2 = HideLambda2 {revealLambda2 :: forall a . PLambda2 a} 




  eval2 :: Lambda2 -> Value
  eval2 e = evalPLambda (revealLambda2 e) where
    evalPLambda :: PLambda2 Value -> Value
    {-"\ldots"-}
    evalPLambda (PFix2 f)           = fix (evalPLambda . f)



    evalPLambda (PBool2 b)          = VBool b
    evalPLambda (PIf2 e1 e2 e3)    =
      case evalPLambda e1 of 
         VBool b  -> if b then evalPLambda e2 else evalPLambda e3
         _        -> error "Need a boolean!"
    evalPLambda (PVar2 v)           = v
    evalPLambda (PLit2 n)           = VLit n
    evalPLambda (PAdd2 e1 e2)       = 
      case (evalPLambda e1, evalPLambda e2) of 
        (VLit x, VLit y) -> VLit (x + y)
        _                -> error "Need 2 integer arguments!"
    evalPLambda (PMult2 e1 e2)      = case  (evalPLambda e1, evalPLambda e2) of 
                                           (VLit x, VLit y)  -> VLit (x * y)
                                           _                 -> error "Need 2 integer arguments!"
    evalPLambda (PEq2 e1 e2)        = case  (evalPLambda e1, evalPLambda e2) of 
                                           (VLit x, VLit y)  -> VBool (x == y)
                                           _                 -> error "Need 2 integer arguments!"
    evalPLambda (PLam2 f)           = Val (evalPLambda . f)
    evalPLambda (PApp2 e1 e2)       = 
      case evalPLambda e1 of 
         Val f  -> f (evalPLambda e2)
         _       -> error "Bad application!"





  fix :: (a -> a) -> a
  fix f = let r = f r in r  -- |f (fix f)|































  fact = PFix2 (\f -> PLam2 (\n -> 
       PIf2  (PEq2 (PVar2 n) (PLit2 0)) 
             (PLit2 1) 
             (PMult2  (PVar2 n) 
                      (PApp2 (PVar2 f) (PAdd2 (PVar2 n) (PLit2 (-1)))))))
 
  test1 = HideLambda2 (PApp2 fact (PLit2 7)) 











  data PLambda3 a = PMulti ([a] -> [PLambda3 a]) | {-"\ldots"-}



      PVar3 a 
   |  PFix3 (a -> PLambda3 a)
   |  PLit3 Int
   |  PBool3 Bool
   |  PIf3 (PLambda3 a) (PLambda3 a) (PLambda3 a)
   |  PMult3 (PLambda3 a) (PLambda3 a)
   |  PEq3 (PLambda3 a) (PLambda3 a) 
   |  PAdd3 (PLambda3 a) (PLambda3 a) 
   |  PLam3 (a -> PLambda3 a) 
   |  PApp3 (PLambda3 a) (PLambda3 a)

  newtype Lambda3 = HideLambda3 {revealLambda3 :: forall a . PLambda3 a} 



  eval3 :: Lambda3 -> Value
  eval3 e = evalPLambda (revealLambda3 e) where
    evalPLambda :: PLambda3 Value -> Value
    {-"\ldots"-}
    evalPLambda (PMulti f)          = head $ fix (map evalPLambda . f)





    evalPLambda (PFix3 f)           = fix (evalPLambda . f)
    evalPLambda (PBool3 b)          = VBool b
    evalPLambda (PIf3 e1 e2 e3)    =
      case evalPLambda e1 of 
         VBool b  -> if b then evalPLambda e2 else evalPLambda e3
         _        -> error "Need a boolean!"
    evalPLambda (PVar3 v)           = v
    evalPLambda (PLit3 n)           = VLit n
    evalPLambda (PAdd3 e1 e2)       = 
      case (evalPLambda e1, evalPLambda e2) of 
        (VLit x, VLit y) -> VLit (x + y)
        _                -> error "Need 2 integer arguments!"
    evalPLambda (PMult3 e1 e2)      = case  (evalPLambda e1, evalPLambda e2) of 
                                           (VLit x, VLit y)  -> VLit (x * y)
                                           _                 -> error "Need 2 integer arguments!"
    evalPLambda (PEq3 e1 e2)        = case  (evalPLambda e1, evalPLambda e2) of 
                                           (VLit x, VLit y)  -> VBool (x == y)
                                           _                 -> error "Need 2 integer arguments!"
    evalPLambda (PLam3 f)           = Val (evalPLambda . f)
    evalPLambda (PApp3 e1 e2)       = 
      case evalPLambda e1 of 
         Val f  -> f (evalPLambda e2)
         _       -> error "Bad application!"



  -- pair :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
  -- pair f g (x,y) = (f x, g y)






























  evenodd :: Lambda3
  evenodd = HideLambda3 (PMulti (\(~(_:odd:even:_)) -> 
    [  PApp3 (PVar3 odd) (PLit3 10),  -- body of |letrec|
       PLam3  (\n ->                  -- definition of |odd|
         PIf3  (PEq3 (PVar3 n) (PLit3 0)) 
               (PBool3 False) 
               (PApp3 (PVar3 even) (PAdd3 (PVar3 n) (PLit3 (-1))))),
       PLam3  (\n ->                  -- definition of |even|
         PIf3  (PEq3 (PVar3 n) (PLit3 0)) 
               (PBool3 True) 
               (PApp3 (PVar3 odd) (PAdd3 (PVar3 n) (PLit3 (-1)))))
    ]))


















































