import           Control.Lens
import           Control.Monad
import           Data.Object
import           Data.Object.Flying
import qualified Data.Object.Wav as Wav
import           Data.Maybe
import qualified Data.Vector as V

x1,x2,x3,x4,x5 :: Object
x1 = empty

x2 = x1 & speed .~ Just 120

x3 = x2 & sound .~ Just "quack! quack. quack? quack..."

x4 = x3 & over speed (fmap (*2)) -- accelerate duck!

x5 = x3 & Wav.sound .~ Just (V.generate 44100 (\i -> floor (sin(2*pi*800 * fromIntegral i/44100))))

main = do
  print x1
  print $ x1 ^. speed
  print x2
  print $ x2 ^. speed
  print $ x3 ^. sound
  let speeders :: [Object]
      speeders = do
        x <- [x1, x2, x3, x4, x5]
        s <- maybeToList $ x ^. speed
        guard $ s > 200
        return x
  print speeders