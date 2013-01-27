import Data.SBV

type SReg = SWord8
type SRegArray = SWord64
type SInst = SWord8

nReg :: Num a => a
nReg = 8

bitPerReg :: Num a => a
bitPerReg = 8


exec :: SInst -> SRegArray -> Symbolic SRegArray
exec inst rs = do
  let opCode = inst `shiftR` 4
      arg = inst .&. 0xf
      lhs = (rs `sbvShiftRight` (bitPerReg * arg)) .&. 0xff
      rsa0 = (rs .&. (-1-0xff)) -- rs with x[0] cleared
      cands =
        [ -- Nop
          rs
          -- Ial
        , (rs .&. (-1-0xf))  .|. (extend $ extend $ extend $ arg)
          -- Iah
        , (rs .&. (-1-0xf0)) .|. (extend $ extend $ extend $ arg `shiftL` 4)
          -- Cpy
        , rsa0 .|. lhs
          -- Xch
        , rsa0 .&. (-1 - (0xff `sbvShiftLeft` (bitPerReg * arg)))
        ]

  return $ select cands rs opCode

data Asm a
  = Nop   -- 00   do nothing
  | Ial a -- 1r   set lower 4 bit of x[0]
  | Iah a -- 2r   set higher 4 bit of x[0]
  | Cpy a -- 3r   x[0] = x[r]
  | Xch a -- 4r   swap(x[0],x[r])
  | Lnd a -- 5r   x[0] &= x[r]
  | Lor a -- 6r   x[0] |= x[r]
  | Lxr a -- 7r   x[0] ^= x[r]
  | Rar a -- 8r   x[0] >> r
  | Ral a -- 9r   x[0] << r
  | Inc a -- Ar   x[r]++
  | Dec a -- Br   x[r]--
  | Add a -- Cr   x[0] += x[r]
  | Sub a -- Dr   x[0] -= x[r]
  | Mul a -- Er   x[0] *= x[r]
  | Fma a -- Fr   x[0] += x[r] * x[r+1]
    deriving (Eq, Show)



main = do
  print "hi"