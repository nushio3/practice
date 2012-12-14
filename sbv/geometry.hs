{-# LANGUAGE ScopedTypeVariables #-}

import Data.SBV

main = do
  (print =<<) $ allSat $ do
    x <- exists "x"
    y <- exists "y"
    constrain $ x^6 + y^6 .== (1::SReal)
    constrain $ 2*x+3*y   .== 1
    return (true :: SBool)

  putStrLn ""

  (print =<<) $ allSat $ do
    x <- exists "x"
    y <- exists "y"
    constrain $ x^2 + y^2 .== (1::SReal)
    constrain $ (x-2)^2 + (y-1)^2 .== 3^2
    return (true :: SBool)

  putStrLn ""

  (print =<<) $ allSat $ do
    x <- exists "x"
    y <- exists "y"
    constrain $ x^2 + y^2 .== (1::SReal)
    constrain $ (x-2)^2 + y^2 .== 1
    return (true :: SBool)

  putStrLn ""

  (print =<<) $ allSat $ do
    x <- exists "x"
    y <- exists "y"
    constrain $ x^2 + y^2 .== (1::SReal)
    constrain $ (x-3)^2 + y^2 .== 1
    return (true :: SBool)