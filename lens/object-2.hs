{-# LANGUAGE RankNTypes #-}
import           Control.Applicative hiding (empty)
import           Control.Lens
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Text.Printf

newtype Obj = Obj { unObj :: Map.Map String Member }
instance Show Obj where
  show obj =
    printf "Obj [%s]" $
    unwords $
    map (\(k,(Member v)) -> printf "%s->%s," k (show $ obj ^? v)) $
    Map.toList $ unObj obj

newtype Member = Member { unMember :: Getter Obj Double}

member :: Simple Lens Obj (Getter Obj Double)
member = lens gettr settr
  where
    gettr

empty :: Obj
empty = Obj $ Map.empty

x :: Obj
x = empty & set (member "price") (to 120)

main = do
  print empty