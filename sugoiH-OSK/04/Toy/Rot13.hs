module Toy.Rot13 where
  
encrypt :: String -> String
encrypt = map go
  where
    go c
      | 'A' <= c && c <= 'Z' = r13 'A' c
      | 'a' <= c && c <= 'z' = r13 'a' c
      | otherwise            = c 

    r13 :: Char -> Char -> Char
    r13 o x = toEnum $ (fromEnum x - fromEnum o + 13) `mod` 26 +  fromEnum o
      
   