{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

import           Control.Applicative ((<$>),pure)
import           Control.Lens
import           Data.Dynamic
import qualified Data.Map as Map

class UseReal a where
  type UnderlyingReal a :: *

class Objective o where
  table :: Simple Lens o Table

newtype Table = Table {unTable :: Map.Map TypeRep Dynamic}


mass :: (Objective o, UnderlyingReal o ~ real) => Simple Traversal o real
mass r2ar objc = pure objc
  where
    obj :: Table
    obj = objc ^. table
    

main = do
  putStrLn "hu"