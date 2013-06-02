{-# LANGUAGE TemplateHaskell #-}

-- | this is a
--   block of haddock comments

import Language.Haskell.Lexer

{-| haddock comments -}

main :: IO ()
main = do
  str <- readFile "test1.hs" {- comments -}
  let lexResult = lexerPass0 str
  print lexResult -- prints lexResult
  putStr $ concat $ map (snd.snd) lexResult

-- | yet another hadock comments.
sub :: IO ()
sub = sub