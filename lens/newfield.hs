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


e, x, e', x' :: Object
e = Nothing
x = e & set olens 42

e' = Nothing
x' = e & set osetter 42

main = do
  print e
  print x
  print e'
  print x'
