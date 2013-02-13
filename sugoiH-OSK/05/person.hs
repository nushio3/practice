{-# LANGUAGE RecordWildCards #-}

data Person =
  Western
  { firstName :: String
  , middleName :: String
  , lastName :: String
  , age :: Int
  , height :: Float
  , phoneNumber :: String
  , flavor :: String } |
  Eastern
  { lastName :: String
  , firstName :: String
  , age :: Int
  , height :: Float
  , phoneNumber :: String
  , flavor :: String } deriving (Eq, Show)

tm :: Person
tm = Eastern "Muranushi" "Takayuki" 29 180 "0757537000" "Maccha"

spj :: Person
spj = Western "Simon" "Peyton" "Jones" 40 190 "316316316" "Strawberry"

ml :: Person
ml = Western
  { firstName = "Miran"
  , lastName = "Lipovaca"
  , flavor = "Chocolate"
  }


main = do
  print tm
  print spj
  print $ ml == spj

  greet spj
  greet tm

greet :: Person -> IO ()
greet Western{..} = do
  putStrLn $ "Hello, Sir " ++ lastName

greet Eastern{..} = do
  putStrLn $ "Domo, " ++ lastName ++ " san."