{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Expr = Imm Int | Add Expr Expr | Mul Expr Expr
                                      deriving (Eq, Show)

makePrisms ''Expr

testExpr :: Expr
testExpr = Add (Imm 2) (Mul (Imm 3) (Imm 5))

main :: IO ()
main = do
  print $ testExpr ^? _Add
  print $ testExpr ^? _Add . _2 . _Mul
  print $ testExpr ^? _Imm
