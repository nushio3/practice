-- Trevor L. McDonell
-- tmcdonell@cse.unsw.edu.au
--
{-# LANGUAGE TypeOperators #-}

import Prelude                                  as P
import Data.Bits
import Data.Array.Accelerate                    as A
import Data.Array.Accelerate.CUDA               as A

{--}
masks :: (Elt a, Num a)
      => Acc (Array DIM1 Word64)
      -> Acc (Array DIM1 Word64)
      -> Acc (Array DIM2 a)
      -> Acc (Array DIM2 a)
masks cols rows table
  = A.generate (shape table)
               (\ix -> let Z :. i :. j  = unlift ix     :: Z :. Exp Int :. Exp Int

                           bit_r        = i `mod` 64
                           bit_c        = j `mod` 64

                           ix_r         = i `div` 64
                           ix_c         = j `div` 64

                           xr           = (rows ! index1 ix_r) `A.testBit` bit_r
                           xc           = (cols ! index1 ix_c) `A.testBit` bit_c
                       in
                       xr &&* xc ? ( table ! ix, constant 0 ))



compute :: Acc (Array DIM1 Word64)
        -> Acc (Array DIM1 Word64)
        -> Acc (Array DIM2 Word32)
        -> Acc (Scalar Word32)

compute cols rows table
  = foldAll xor 0 (masks cols rows table)



--}

{--
mask :: Exp Word64
     -> Exp Word64
     -> Acc (Array DIM2 Word32) -- 64 x 64
     -> Exp Word32
mask rows cols block
  = let foo =
          A.iterate
            (constant 64)
            (\v -> let (i, acc)  = unlift v :: (Exp Int, Exp Word32)

                       acc' = A.snd $ A.iterate (constant 64)
                                             (\u -> let (j, acc) = unlift u :: (Exp Int, Exp Word32)
                                                        acc'     = bar i j acc

                                                    in lift (j+1, acc') :: Exp (Int, Word32))
                                             (lift (constant 0, acc) :: Exp (Int, Word32))

                    in
                    lift (i+1, acc'))
            (constant (0,0) :: Exp (Int, Word32))



        bar i j acc
          = let r = rows `A.testBit` i
                c = cols `A.testBit` j

                x = r &&* c ? ( acc `xor` block ! (index2 i j) , acc )
            in
            x

    in
    A.snd foo

--}

col :: Acc (Vector Word64)
col = use $ fromList (Z :. 1) [1] :: Acc (Vector Word64)

row :: Acc (Vector Word64)
row = use $ fromList (Z :. 1) [0x0100b] :: Acc (Vector Word64)

tab :: Acc (Array DIM2 Word32)
tab = use (fromList (Z :. 64 :. 64) [0..]) :: Acc (Array DIM2 Word32)


go = run (compute col row tab)

