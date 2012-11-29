import Control.Applicative

main :: IO ()
main = putStrLn "Hello"


earthMass, sunMass, marsMass :: [Double]
earthMass = [1,10,100]
sunMass = (*) <$>  [9,10,11] <*> earthMass
marsMass = (*) <$> [0.09,0.1,0.11] <*> earthMass

sunPerMars = (/) <$> sunMass <*> marsMass
sunPerMars_range = (minimum sunPerMars, maximum sunPerMars)

