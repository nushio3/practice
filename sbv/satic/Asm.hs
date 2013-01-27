module Asm where

import Data.SBV

type SReg = SWord8
type SRegArray = [SWord8]
type SInst = SWord8

nReg :: Num a => a
nReg = 16

bitPerReg :: Num a => a
bitPerReg = 8


exec :: SInst -> SRegArray -> Symbolic SRegArray
exec inst rs = do
  let opCode = inst `shiftR` 4
      arg  = inst .&. 0xf
      lhs  = select rs 0 arg
      lhs2 = select rs 0 ((arg+1) `sDiv` nReg)

      rsTail = tail rs
      rsA = head rs
      xchResults :: [SRegArray]
      xchResults =
        [ (lhs : ) $ drop 1 $
          take i rs ++ [rsA] ++ drop (i+1) rs
        | i<-[0..nReg-1]
        ]
      cands =
        [ -- Nop
          rs
        , -- Ial
          ((rsA .&. 0xf0) .|. arg) : rsTail
        , -- Iah
          ((rsA .&. 0x0f) .|. (arg `shiftL` 4)) : rsTail
        , -- Cpy
          lhs : rsTail
        , -- Xch
          select xchResults rs arg
        , -- Lnd
          (rsA .&. lhs) : rsTail
        , -- Lor
          (rsA .|. lhs) : rsTail
        , -- Lxr
          (rsA `xor` lhs) : rsTail
        , -- Sar
          (rsA `sbvShiftRight` arg) : rsTail
        , -- Sal
          (rsA `sbvShiftLeft` arg) : rsTail
        , -- Inc
          (rsA + arg) : rsTail
        , -- Dec
          (rsA - arg) : rsTail
        , -- Add
          (rsA + lhs) : rsTail
        , -- Sub
          (rsA - lhs) : rsTail
        , -- Mul
          (rsA * lhs) : rsTail
        , -- Fma
          (rsA + lhs*lhs2) : rsTail
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
  | Sar a -- 8r   x[0] >> r
  | Sal a -- 9r   x[0] << r
  | Inc a -- Ar   x[0] += r
  | Dec a -- Br   x[0] -= r
  | Add a -- Cr   x[0] += x[r]
  | Sub a -- Dr   x[0] -= x[r]
  | Mul a -- Er   x[0] *= x[r]
  | Fma a -- Fr   x[0] += x[r] * x[r+1]
    deriving (Eq, Show)

toHex :: (Bits a, Num a) => Asm a -> a
toHex inst = case inst of
  Nop -> 0
  Ial x -> 0x10 .|. (0xf .&. x)
  Iah x -> 0x20 .|. (0xf .&. x)
  Cpy x -> 0x30 .|. (0xf .&. x)
  Xch x -> 0x40 .|. (0xf .&. x)
  Lnd x -> 0x50 .|. (0xf .&. x)
  Lor x -> 0x60 .|. (0xf .&. x)
  Lxr x -> 0x70 .|. (0xf .&. x)
  Sar x -> 0x80 .|. (0xf .&. x)
  Sal x -> 0x90 .|. (0xf .&. x)
  Inc x -> 0xa0 .|. (0xf .&. x)
  Dec x -> 0xb0 .|. (0xf .&. x)
  Add x -> 0xc0 .|. (0xf .&. x)
  Sub x -> 0xd0 .|. (0xf .&. x)
  Mul x -> 0xe0 .|. (0xf .&. x)
  Fma x -> 0xf0 .|. (0xf .&. x)
