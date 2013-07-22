{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import UnitTH

x :: [ut| hoge |] Int
x = [ut| huga |] 3

main :: IO ()
main = print x