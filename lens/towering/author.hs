{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Lens.Setter
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid

newtype LaTeX = LaTeX { _laText :: String}
  deriving (Eq, Show)

deriving instance Monoid LaTeX

makeClassy ''LaTeX

prog :: StateT LaTeX IO ()
prog = do
  laText .= "Hi"
  
prog2 :: WriterT LaTeX IO ()
prog2 = do
  tell $ LaTeX "Hoho"
  scribe laText "HaHa"

main :: IO ()
main = do
  ((),ret) <- runStateT prog (LaTeX "")
  print ret
  ((),ret2) <- runWriterT prog2
  print ret2
  
  