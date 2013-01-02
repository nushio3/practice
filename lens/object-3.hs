{-# LANGUAGE RankNTypes #-}
import           Control.Applicative hiding (empty)
import           Control.Lens
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Text.Printf

newtype Obj = Obj { unObj :: Map.Map String (Obj->Double) }

instance Show Obj where
  show obj =
    printf "Obj {%s}" $
    intercalate ", " $
    map (\(k,f) -> printf "%s->%s" k (show $ f obj)) $
    Map.toList $ unObj obj



empty ,x ,y :: Obj
empty =Obj $ Map.empty

x = Obj $ Map.insert "price" (const 120) $ unObj empty

y = Obj $ Map.insert "speed" (const 3500) $ unObj x

main = do
  print empty
  print x
  print y

