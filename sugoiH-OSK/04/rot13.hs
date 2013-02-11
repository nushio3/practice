import qualified Toy.Rot13 as Rot13 (encrypt)

testRot13 :: Bool
testRot13 = Rot13.encrypt chipher == plain
            -- 暗号文をさらにencryptすると平文に戻るはず
plain = "Lern You a Haskell for Great Good!" -- 平文

chipher = Rot13.encrypt plain -- 暗号文

main = do
  putStrLn chipher
  print $ testRot13