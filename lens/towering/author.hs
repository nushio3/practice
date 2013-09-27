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

data Document = Document { _docLaTeX :: LaTeX, _docSize :: Int}
  deriving (Eq, Show)                
                
makeClassy ''Document
instance HasLaTeX Document where laTeX = docLaTeX

instance Monoid Document where
  mempty = Document (LaTeX "") 0
  mappend (Document s0 n0) (Document s1 n1) 
    = Document (mappend s0 s1) (n0 + n1)
  



prog :: StateT LaTeX IO ()
prog = do
  laText .= "Hi"
  
prog2 :: WriterT LaTeX IO ()
prog2 = do
  tell $ LaTeX "From writer monad."
  scribe laTeX $ LaTeX "From lens with LaTeX."  
  scribe laText "From lens with String."

prog3 :: (HasLaTeX s, MonadWriter s m) => m ()
prog3 = do
  scribe laText "To MonadWriter from lens with String."  

main :: IO ()
main = do
  ((),ret) <- runStateT prog (LaTeX "")
  print ret
  ((),ret2) <- runWriterT prog2
  print ret2
  ((), ret3) <- runWriterT prog3
  print (ret3 :: LaTeX)
  ((), ret4) <- runWriterT prog3
  print (ret4 :: Document)
  
  