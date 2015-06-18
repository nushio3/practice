{-# LANGUAGE GADTs,StandaloneDeriving,FlexibleInstances,FlexibleContexts #-}

module IR where

import Compiler.Hoopl
import Unsafe.Coerce


type Var = String

data Binop = Add | Mul
                   deriving (Eq, Show)
data Triop = FMA
                   deriving (Eq, Show)

data Expr = Lit Double
          | Load Var
          | Binop Binop Expr Expr
          | Triop Triop Expr Expr Expr
                   deriving (Eq, Show)


data Proc = Proc { name :: String, body :: Graph Insn C C }

data Insn e x where
  Entry  :: [Var]  ->                               Insn C O
  Assign :: Var    -> Expr    ->                    Insn O O
  Return :: [Expr] ->                               Insn O C

instance NonLocal Insn where
  entryLabel (Entry _) = unsafeCoerce (0 :: Int)
  successors (Return _) = []


deriving instance Show (Insn e x)
