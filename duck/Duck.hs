import qualified Data.Map as Map
import           Data.Dynamic

module Duck where

newtype Object = Object{Map.Map TypeRep Dynamic}