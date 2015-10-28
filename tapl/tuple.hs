import qualified Data.Map as M

type Identifier = String

data LExpr = Scalar Identifier | LTuple [LExpr]
           deriving (Eq, Show)
data Expr = Imm Int | RTuple [Expr] | Add Expr Expr
          deriving (Eq, Show)

data NormalForm = NFImm Int | NTuple [NormalForm]
          deriving (Eq, Show)

data Statement = LExpr := Expr
          deriving (Eq, Show)

data Program = Program [Statement]
          deriving (Eq, Show)

type Semantics = M.Map Identifier NormalForm


eval :: Program -> Semantics
eval prog = ret
  where
    stmts :: [Statement]
    Program stmts = prog

    ret :: Semantics
    ret = M.unions $ map evalStmt stmts

    evalStmt :: Statement -> Semantics
    evalStmt (lhs := rhs) = match lhs $ evalExpr rhs


    match :: LExpr -> NormalForm -> Semantics
    match (Scalar ident) nf  = M.singleton ident nf
    match (LTuple _) (NFImm _) = error "match tuple v.s. scalar"
    match (LTuple lexprs) (NTuple nfs)
      | length lexprs /= length nfs = error "tuple type error"
      | otherwise = M.unions $ zipWith match lexprs nfs

    evalExpr :: Expr -> NormalForm
    evalExpr (Imm x) = NFImm x
    evalExpr (Add a b)

main :: IO ()
main = do
  print $ eval testProg

testProg :: Program
testProg = Program
  [ Scalar "a" := Add (Imm 2) (Imm 40)
  ]
