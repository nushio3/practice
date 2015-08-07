{-# LANGUAGE ScopedTypeVariables #-}

推理 :: forall 新幹線 在来線 米原を通る.
  (Either 新幹線 在来線, 新幹線 -> 米原を通る, 在来線 -> 米原を通る) -> 米原を通る
推理 (x :: Either 新幹線 在来線,
      p ::新幹線 -> 米原を通る,
      q :: 在来線 -> 米原を通る) =
  case x of
    Left  (y :: 新幹線) -> (p y :: 米原を通る)
    Right (z :: 在来線) -> (q z :: 米原を通る)

main :: IO ()
main = print "hi"
