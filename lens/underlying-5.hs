{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Applicative ((<$>),pure)
import qualified Control.Category as Cat ((.))
import           Control.Lens
import           Control.Lens.Iso
import           Data.Dynamic
import           Data.HList.FakePrelude
import           Data.HList.HListPrelude
import           Data.HList.Record
import           Data.Ratio
import qualified Data.Map as Map


class (Typeable (UnderlyingReal a)) => UseReal a  where
  type UnderlyingReal a :: *

class (Typeable (UnderlyingInteger a)) => UseInteger a  where
  type UnderlyingInteger a :: *


newtype Table = Table {unTable :: Map.Map TypeRep Dynamic}

class Objective o where
  table :: Simple Iso o Table
  tableMap :: Simple Iso o (Map.Map TypeRep Dynamic)
  tableMap = table Cat.. (iso unTable Table)
-- Cat with an even longer tail =>  Cat....
--  /\ /\
-- (=^x^=)...   < Meow!!
--   ||  ||

empty :: Objective o => o
empty = Table Map.empty ^. from table

class (Objective o)
      => Member o memb where
  type ValType o memb :: *

memberLens :: (Objective o, Member o memb,
             Typeable memb, Typeable (ValType o memb))
     => memb -> Simple Traversal o (ValType o memb)
memberLens memb0 r2ar obj = case Map.lookup tag (unTable tbl) of
  Just dr -> case fromDynamic dr of
    Just r -> (\r' -> obj & over tableMap
          (Map.insert tag (toDyn r')) ) <$> r2ar r
    Nothing -> pure obj
  Nothing -> pure obj
  where
    tbl :: Table
    tbl = obj ^. table
    tag :: TypeRep
    tag = typeOf memb0


insert :: (Objective o, Member o memb, ValType o memb ~ val,
           Typeable memb, Typeable val)
  => memb -> val -> o -> o
insert memb0 val0 = over tableMap $ Map.insert tag (toDyn val0)
  where
    tag :: TypeRep
    tag = typeOf memb0


type MemberLens memb =
  (Member o memb, Typeable (ValType o memb))
        => Simple Traversal o (ValType o memb)

data Mass = Mass deriving Typeable
instance (Objective o) => Member o Mass where
  type ValType o Mass = (UnderlyingReal o)
mass :: MemberLens Mass
mass = memberLens Mass




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


newtype Object u = Object {unObject :: Table}
instance Objective (Object u) where
  table = iso unObject Object
instance  UseReal u => UseReal (Object u) where
  type UnderlyingReal (Object u) = UnderlyingReal u
instance  UseInteger u => UseInteger (Object u) where
  type UnderlyingInteger (Object u) = UnderlyingInteger u


x :: forall o real. (Objective o, UnderlyingReal o ~ real, Typeable real,
                     Num real) => o
x = empty
  & insert Mass 2

main = do
  putStrLn "hu"
  print $ (fmap (+40)) $ (x::ObjectR) ^? mass
  print $ (fmap (*2e20)) $ (x::ObjectD) ^? mass