{-# LANGUAGE ConstraintKinds, DataKinds, DeriveFoldable, DeriveFunctor,
DeriveTraversable, FlexibleContexts, FlexibleInstances,
FunctionalDependencies, GADTs, KindSignatures, MultiParamTypeClasses,
PatternSynonyms, RankNTypes, ScopedTypeVariables, StandaloneDeriving,
TemplateHaskell, TupleSections, TypeFamilies, TypeOperators,
UndecidableInstances, ViewPatterns #-}

{-

Built based on modular reifiable matching by Bruno C. d. S. Oliveira, Shin-Cheng Mu, and Shu-Hung You.
http://www.iis.sinica.edu.tw/~scm/2015/mrm/

see also: composable algebra by Bruno & Boya http://emmabypeng.github.io/files/FYP_part2.pdf

Tested on stack lts-3.11
-}

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Monoid hiding (Sum)
import qualified Data.Set as S
import Data.Traversable
import qualified Text.Trifecta as P hiding (string)
import qualified Text.Parser.Expression as P
import qualified Text.PrettyPrint.ANSI.Leijen as Ppr hiding (line, (<>), (<$>), empty, integer)
import System.IO

-- The fix point of F-algebra, with parent search
data Fix f where
  In :: Functor f => {_metadata :: Maybe Metadata, _out :: f (Fix f)} -> Fix f

instance (Show (f (Fix f))) => Show (Fix f) where
--  showsPrec n (In Nothing x)  = showsPrec n x
--  showsPrec n (In (Just t) x) = showsPrec n (x,t)
    showsPrec n (In _ x) = showsPrec n x

metadata :: Functor f => Lens' (Fix f) (Maybe Metadata)
metadata fun (In p o) = fmap (\p' -> In p' o) (fun p)

fix :: forall f. Functor f => Iso' (Fix f) (f (Fix f))
fix = iso _out go
  where
    go :: f (Fix f) -> Fix f
    go ffixf = In Nothing ffixf


-- The sum of functors
data Sum (fs :: [* -> *]) x where
  Void :: Sum '[] x
  Here :: Traversable f => f x -> Sum (f ': fs) x
  There :: Sum fs x -> Sum (f ': fs) x

instance Show x => Show (Sum '[] x) where
  show Void = "∅"

instance (Show (f x), Show (Sum fs x)) => Show (Sum (f ': fs) x) where
  showsPrec n (Here x) = showsPrec n x
  showsPrec n (There x) = showsPrec n x

instance Functor (Sum fs) where
  fmap _ Void      = Void
  fmap f (Here t)  = Here $ fmap f t
  fmap f (There t) = There $ fmap f t

instance Foldable (Sum fs) where
  foldMap = foldMapDefault

instance Traversable (Sum fs) where
  traverse afb Void      = pure Void
  traverse afb (Here x)  = Here  <$> traverse afb x
  traverse afb (There x) = There <$> traverse afb x

-- The prisms for accessing here and there
_Here :: Traversable f => Prism' (Sum (f ': fs) x) (f x)
_Here = let a :: Sum (f ': fs) x -> Maybe (f x)
            a (Here x) = Just x
            a _        = Nothing
    in prism' Here a

_There :: Traversable f => Prism' (Sum (f ': fs) x) (Sum fs x)
_There = let a :: Sum (f ': fs) x -> Maybe (Sum fs x)
             a (There x) = Just x
             a _         = Nothing
    in prism' There a


-- The constraint that functor f is an element of a functor set fs
class Elem f fs where
  constructor :: Prism' (Sum fs x) (f x)

instance {-# OVERLAPPING #-} Traversable f => Elem f (f ': fs) where
  constructor = _Here
instance {-# OVERLAPPABLE #-} (Traversable f, Traversable g, Elem f fs) => Elem f (g ': fs) where
  constructor = _There . constructor

-- The constraint that set of functors fs is a subset of gs
class Subset fs gs where
  subrep :: Prism' (Sum gs x) (Sum fs x)

instance {-# OVERLAPPING #-} Subset '[] '[] where
  subrep = simple

instance {-# OVERLAPPING #-} Subset '[] fs => Subset '[] (f ': fs) where
  subrep = prism' There (const Nothing) . subrep

instance {-# OVERLAPPABLE #-} (Traversable f, Elem f gs, Subset fs gs) => Subset (f ': fs) gs where
  subrep = let fwd :: Sum (f ': fs) x -> Sum gs x
               fwd (Here x)  = review constructor x
               fwd (There x) = review subrep x

               bwd :: Sum gs x -> Maybe (Sum (f ': fs) x)
               bwd ((^? constructor ) -> Just x) = Just (Here x)
               bwd ((^? subrep) -> Just x) = Just (There x)
               bwd _                     = Nothing
           in prism' fwd bwd

-- The constraint that object x can somehow be matched to functor f
class Matches f x where
  type Content f x :: *
  match :: Prism' x (f (Content f x))

instance Matches f (f x) where
  type Content f (f x) = x
  match = simple

instance Elem f fs => Matches f (Sum fs x) where
  type Content f (Sum fs x) = x
  match = constructor

instance Elem f fs => Matches f (Fix (Sum fs)) where
  type Content f (Fix (Sum fs)) = Fix (Sum fs)
  match = fix . constructor

-- emulate some mrm
type MRM_Matches fs a b = Sum fs a -> b

extractAt :: Elem f fs => (MRM_Matches fs a b) -> (f a -> b)
extractAt sfun fa = sfun $ (review constructor) fa

-- We create languages by folding over set of functors
type Lang (fs :: [ * -> * ]) = Fix (Sum fs)
type MatchPrism (f :: * -> *) = forall f x . Matches f x => Prism' x (f (Content f x))

type Algebrogen f a b = f a -> b
type Algebra f a = f a -> a
type AlgebraM m f a = f a -> m a

foldout :: Algebra f a -> Fix f -> a
foldout k (In _ x) = k $ fmap (foldout k) x

fold :: Algebra f (Lang g) -> Fix f -> (Lang g)
fold k (In meta x) = (k $ fmap (fold k) x) & metadata .~ meta


mlift :: (Monad m, Traversable fs) => Algebrogen fs a b -> Algebrogen fs (m a) (m b)
mlift fsa2b fsma = liftM fsa2b $ sequence fsma

mfold :: (Monad m, Traversable f) => AlgebraM m f (Lang g) -> Fix f -> m (Lang g)
mfold k (In meta x) = do
  r1 <- traverse (mfold k) x
  r2 <- k r1
  return $ r2 & metadata .~ meta

mfoldout :: Monad m => (Sum fs a -> m a) -> Lang fs -> m a
mfoldout k x = foldout (join . mlift k) x

subFix :: (Subset fs gs) => Lang fs -> Lang gs
subFix = fold (review (fix . subrep))

subOp :: (Subset fs gs) => (Lang gs -> c) -> Lang fs -> c
subOp g = g . subFix

transAlg :: Subset fs gs => Algebra (Sum fs) (Lang gs)
transAlg = review (fix . subrep)

mTransAlg :: (Monad m, Subset fs gs) => AlgebraM m (Sum fs) (Lang gs)
mTransAlg = return . transAlg


(+::) :: Algebrogen f a b -> Algebrogen (Sum fs) a b -> Algebrogen (Sum (f ': fs)) a b
af +:: afs = affs
  where
    affs (Here x)  = af  x
    affs (There x) = afs x

(>::) :: Elem f fs => Algebra f a -> Algebra (Sum fs) a -> Algebra (Sum fs) a
af >:: afs= affs
  where
    affs ((^? constructor) -> Just fa) = af  fa
    affs x                             = afs x


infixr 5 +::, >::

-- ========================== --
-- ==== Example language ==== --
-- ========================== --



-- == The Value Functor ==
data ValueF x = ValueF Int
             deriving (Eq, Ord, Functor, Foldable, Traversable)
instance Show (ValueF x) where
  showsPrec p (ValueF n) = showsPrec p n

value :: MatchPrism ValueF
value = match
-- smart patterns
pattern Value n <- ((^? value) -> Just (ValueF n)) where
  Value n = value # ValueF n

-- == The Tuple Functor ==
data TupleF x = TupleF [x]
             deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
tuple :: MatchPrism TupleF
tuple = match

-- smart patterns
pattern Tuple xs <- ((^? tuple) -> Just (TupleF xs)) where
  Tuple xs = tuple # TupleF xs

-- == The Arithmetic Functor ==
data ArithF x = ImmF Int | AddF x x | MulF x x
             deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
arith :: MatchPrism ArithF
arith = match
-- smart patterns
pattern Imm n <- ((^? arith) -> Just (ImmF n)) where
  Imm n = arith # ImmF n
pattern Add a b <- ((^? arith) -> Just (AddF a b)) where
  Add a b = arith # AddF a b
pattern Mul a b <- ((^? arith) -> Just (MulF a b)) where
  Mul a b = arith # MulF a b


-- == The Compiler Metadata Functor ==
data Metadata = Metadata {_metadataRendering :: P.Rendering}
instance Show Metadata where
  show = const ""

-- == compiler monad == --

type M = Either Ppr.Doc

-- == type synonyms and evaluation ==

evArith :: AlgebraM M ArithF NF
evArith (Imm n)    = return $ Value n
evArith (Add a b)  = evBinOp (+) a b
evArith (Mul a b)  = evBinOp (*) a b

evBinOp :: (Int -> Int -> Int) -> NF -> NF -> M NF
evBinOp op a b = case (a,b) of
   (Tuple xs, Tuple ys) | length xs == length ys ->
                                Tuple <$> zipWithM (evBinOp op) xs ys
   (Tuple _, Tuple _) -> Left $
                         let Just (Metadata ma) = a ^. metadata
                             Just (Metadata mb) = b ^. metadata
                         in
                           P.explain (P.addCaret (mb ^. P.renderingDelta) $ P.addCaret (ma ^. P.renderingDelta) ma) $
                           P.Err (Just $ Ppr.text "tuple length mismatch") [] (S.empty)

   (Value x, ys) -> eval1 (op x) ys
   (xs, Value y) -> eval1 (flip op y) xs
   (Value x, Value y) -> return $  Value (op x y)

eval1 :: (Int -> Int) -> NF -> M NF
eval1 f xs = case xs of
  Value n ->  return $ Value (f n)
  Tuple ys -> Tuple <$> mapM (eval1 f) ys


type Expr = Lang [ArithF,TupleF]
type NF   = Lang [ValueF,TupleF]

expr1 :: Expr
expr1 = Tuple [Imm 23 `Add` Imm 21, Imm 4]

eval :: Expr -> M NF
eval = mfold $ evArith +:: mTransAlg

-- == Parser ==

-- parsers
withMeta :: P.Parser (Lang f) -> P.Parser (Lang f)
withMeta p = do
  r <- P.rend
  x <- p
  return $ x & metadata .~ (Just $ Metadata r)


parseImm :: (Elem ArithF gs) => P.Parser (Lang gs)
parseImm = do
  i <- P.integer
  return $ Imm $ fromInteger i

parseExpr :: P.Parser Expr
parseExpr = P.buildExpressionParser tbl parseTerm
  where
    tbl = [[binary "*" Mul P.AssocLeft],
           [binary "+" Add P.AssocLeft]
           ]
    binary name fun assoc = P.Infix (fun <$ P.symbol name) assoc

parseTuple :: P.Parser Expr
parseTuple = withMeta $ do
  _ <- P.symbol "("
  xs <- P.sepBy parseExpr (P.symbol ",")
  _ <- P.symbol ")"
  case xs of
   [x] -> return x
   _ -> return $ Tuple xs

parseTerm :: P.Parser Expr
parseTerm = parseTuple <|> parseImm

main :: IO ()
main = do
  Just exprs <- P.parseFromFile (many parseExpr) "input.txt"
  forM_  exprs $ \expr -> do
    print expr
    case eval expr of
      Right r -> putStrLn $ " -> " ++ show r
      Left doc -> Ppr.displayIO stdout $ Ppr.renderPretty 0.8 80 $ doc <> Ppr.linebreak
