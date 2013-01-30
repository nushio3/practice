import Parupunte
-- import Prelude hiding ((+),(*))
import Prelude (print, ($))
import qualified Prelude 

main = do
     print $ (2+8)*(3+7)
     print $ (2 Prelude.+ 8) Prelude.* (3 Prelude.+ 7)
