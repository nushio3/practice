import BrinkALP
-- import RandomALP
import VolkovALP

import qualified Data.Vector as V
import Data.Vector ((!))

import Projection

import Control.Monad
import Text.Printf

main :: IO ()
main = do
  let alp = makeGaussALP 0.0 0.0 0.0
  forM_ [1..15] $ \n -> do
    let r = 0.5 + 0.5 * (n - 1)
        alp' = makeGaussALP 0.0 0.0 r
        wf = fixCMPosition $ V.fromList [alp,alp']

        (vtot,_) = wf `hVolkov` wf
--        (vtotp,wp) = parity_proj hVolkov (+) wf wf
--        (vtotn,wn) = parity_proj hVolkov (-) wf wf
        (vtotp, wp) = am_proj_0 hVolkov (+) wf wf
    printf "%.2f %.6f %.6f\n" r vtot (vtotp/wp)

