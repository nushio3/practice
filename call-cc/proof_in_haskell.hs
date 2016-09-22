{-# LANGUAGE RankNTypes, TypeOperators #-}

data Bot

type Not a = a -> Bot
type a ∧ b = (a,b)
type a ∨ b = Either a b

admitted = admitted

ドモルガン :: Not (a ∨ b) -> (Not a ∧ Not b)
ドモルガン = admitted

-- 下記の３つはどれも同値で、どれかからどれかは導ける
type P二重否定の除去 = forall a.  ((a -> Bot) -> Bot) -> a
type P排中律 = forall b. Either b (b -> Bot)
type Pパース則 = forall p q. ((p->q)->p)->p



二重否定の除去から排中律 :: P二重否定の除去 -> P排中律
二重否定の除去から排中律 = (\hbde -> hbde (\hana -> hana (Right (\h -> hana (Left h)))))


main :: IO ()
main = putStrLn "Q. E. D."
