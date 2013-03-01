{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BS
import           GHC.Generics

data Tree 
  = BookMark { name :: String, url :: String }
  | Folder   { name :: String, children :: [Tree] }  
  deriving (Eq, Ord, Show, Generic)

instance ToJSON Tree 

main = do
  BS.putStrLn $ encodePretty' defConfig{confIndent = 2} myBookmarks
  print myBookmarks
  print $ at ["Root"] (const $ Folder "" []) myBookmarks

  let lyh = BookMark "funniestsite" "www.learnyou.org"

  print $ at ["Root","Haskell"] 
    (\bkm -> bkm{children = children bkm ++ [lyh]}) 
    myBookmarks
     



myBookmarks :: Tree
myBookmarks = 
  Folder "Root"
    [ Folder "Haskell" 
      [ BookMark "hackage" "www.hackage.org"
      , BookMark "hoogle" "www.hoogle.com"
      ]
    , Folder "My Company"  
      [ BookMark "Hakubi Center" "www.hakubi.kyoto-u.ac.jp"
      ]
    ]


at :: [String] -> (Tree -> Tree) -> Tree -> Tree
at [] f bkm = bkm
at [x] f bkm
  | name bkm == x = f bkm
  | otherwise     = bkm

at (x:xs) f bkm@BookMark{} = bkm
at (x:xs) f bkm@Folder{..} 
  | name == x = bkm{children = map (at xs f) children}
  | otherwise = bkm


            

