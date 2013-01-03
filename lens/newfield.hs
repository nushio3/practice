import Control.Lens

type Object = Maybe Int

olens :: Simple Lens Object Int
olens = lens gettr settr
  where
    gettr (Just x)  = x
    gettr (Nothing) = error "not ready."
    settr _ x = Just x

osetter :: Simple Setter Object Int
osetter = cloneLens olens


e, x, x', y, z :: Object
e = Nothing
x = e & set olens 42
x' = e & set osetter 42

y = x & over olens (*2)
z = e & over olens (*2)




main = do
  print e   -- Nothing
  print x   -- Just 42
  print x'  -- Just 42
  print y   -- Just 84
  print z   -- newfield.hs: not ready.
