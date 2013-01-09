import           Control.Lens
import           Control.Monad
import           Data.List (isInfixOf)
import           Data.Object
import           Data.Object.Flying
import qualified Data.Object.Wav as Wav
import           Data.Maybe
import qualified Data.Vector as V

x1,x2,x3,x4,x5, santa :: Object
x1 = empty

x2 = x1 & speed .~ Just 120

x3 = x2 & sound .~ Just "quack! quack. quack? quack..."

x4 = x3 & over speed (fmap (*2)) -- accelerate duck!

x5 = x2 & Wav.sound .~ Just (V.generate 44100 (\i -> floor (sin(2*pi*800 * fromIntegral i/44100))))

santa = empty
 & speed .~ Just 64000
 & sound .~ Just "Merry Xmas! Hohohoho!"

main = do
  print x1
  print $ x1 ^. speed
  print x2
  print $ x2 ^. speed
  print $ x3 ^. sound
  let speeders :: [Object]
      speeders = do
        x <- [x1, x2, x3, x4, x5, santa]
        sp <- maybeToList $ x ^. speed
        snd <- maybeToList $ x ^. sound
        guard $ sp < 200
        guard $ "quack" `isInfixOf` snd
        return x
  print speeders