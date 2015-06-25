{-# LANGUAGE GADTs,StandaloneDeriving,FlexibleInstances,FlexibleContexts,RankNTypes,ScopedTypeVariables #-}

module IR where

import Compiler.Hoopl
import Unsafe.Coerce
import qualified Data.Set as S

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

varIn :: Expr -> S.Set Var
varIn x = case x of
  Lit _ -> S.empty
  Load v -> S.singleton v
  Binop _ a b -> S.union (varIn a) (varIn b)
  Triop _ a b c -> S.union (varIn a) $ S.union (varIn b) (varIn c)


data Proc = Proc { name :: String, body :: Graph Insn C C }

data Insn e x where
  Entry  :: [Var]  ->                               Insn C O
  Assign :: Var    -> Expr    ->                    Insn O O
  Return :: [Expr] ->                               Insn O C

onlyOneLabel :: Label
onlyOneLabel = unsafeCoerce (0 :: Int)

instance NonLocal Insn where
  entryLabel (Entry _) = onlyOneLabel
  successors (Return _) = []


deriving instance Show (Insn e x)


type Live = S.Set Var
liveLattice :: DataflowLattice Live
liveLattice = DataflowLattice
  { fact_name = "Live variables"
  , fact_bot  = S.empty
  , fact_join = add
  }
    where add _ (OldFact old) (NewFact new) = (ch, j)
            where
              j = new `S.union` old
              ch = changeIf (S.size j > S.size old)

liveness :: BwdTransfer Insn Live
liveness = mkBTransfer live
  where
    live :: Insn e x -> Fact x Live -> Live
    live (Return x) _ = S.unions $ map varIn x
    live (Assign v e) f
      | v `S.member` f = S.union f (varIn e)
      | otherwise      = f
    live (Entry _) f = f


deadCodeElim :: forall m . FuelMonad m => BwdRewrite m Insn Live
deadCodeElim = mkBRewrite d
  where
    d :: Insn e x -> Fact x Live -> m (Maybe (Graph Insn e x))
    d (Assign x _) live
        | not (x `S.member` live) = return $ Just emptyGraph
    d _ _ = return Nothing

livePass :: BwdPass M Insn Live
livePass = BwdPass{
  bp_lattice = liveLattice,
  bp_transfer = liveness,
  bp_rewrite = deadCodeElim
                  }


type M = SimpleFuelMonad


optimize :: Proc -> Proc
optimize proc = runSimpleUniqueMonad $ runWithFuel 9999 $ do
  (g,_,_) <- analyzeAndRewriteBwd livePass (JustC [onlyOneLabel]) (body proc) mapEmpty
  return proc{body=g}
