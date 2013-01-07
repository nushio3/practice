{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Applicative ((<$>),pure)
import qualified Control.Category as Cat ((.))
import           Control.Lens
import           Control.Lens.Iso
import           Data.Dynamic
import           Data.Ratio
import qualified Data.Map as Map

class UseReal a where
  type UnderlyingReal a :: *

newtype Table = Table {unTable :: Map.Map TypeRep Dynamic}

class Objective o where
  table :: Simple Iso o Table
  tableMap :: Simple Iso o (Map.Map TypeRep Dynamic)
  tableMap =
    let iso' :: Simple Iso Table (Map.Map TypeRep Dynamic)
        iso' = iso unTable Table
    in table Cat.. iso'
-- Cat with an even longer tail =>  Cat....
--  /\ /\
-- (=^x^=)...   < Meow!!
--   ||  ||

empty :: Objective o => o
empty = Table Map.empty ^. from table

class Member o memb where
  type ValType o memb :: *

data Mass = Mass deriving Typeable

mass :: (Objective o, UnderlyingReal o ~ real, Typeable real)
     => Simple Traversal o real
mass r2ar obj = case Map.lookup tag (unTable tbl) of
  Just dr -> case fromDynamic dr of
    Just r -> (\r' -> obj & over tableMap
          (Map.insert tag (toDyn r')) ) <$> r2ar r
    Nothing -> pure obj
  Nothing -> pure obj
  where
    tbl :: Table
    tbl = obj ^. table
    tag :: TypeRep
    tag = typeOf Mass



insert :: (Objective o, UnderlyingReal o ~ real, Typeable real)
  => real -> o -> o
insert x = over tableMap $ Map.insert tag (toDyn x)
  where
    tag :: TypeRep
    tag = typeOf Mass

newtype ObjectD = ObjectD {unObjectD :: Table}
instance UseReal ObjectD where
  type UnderlyingReal ObjectD = Double
instance Objective ObjectD where
  table = iso unObjectD ObjectD

newtype ObjectR = ObjectR {unObjectR :: Table}
instance UseReal ObjectR where
  type UnderlyingReal ObjectR = Rational
instance Objective ObjectR where
  table = iso unObjectR ObjectR

x :: forall o real. (Objective o, UnderlyingReal o ~ real, Typeable real, Num real) => o
x = empty
  & insert (2 :: real)

main = do
  putStrLn "hu"
  print $ (fmap (+40)) $ (x::ObjectR) ^? mass
  print $ (fmap (*2e20)) $ (x::ObjectD) ^? mass