-- https://gist.github.com/akanehara/4509540
-- 
-- 問題4
-- ここに、こんな２項演算子のリストがあります。
--
-- [(-), (div), (^)]
--
-- このリストの中の演算子の左辺に2を、 右辺に3を
-- 適用した結果のリストを得るには、map関数と
-- ラムダ式を使ったこんな方法が考えられます。
--
-- map (\f -> 2 `f` 3) [(-), (div), (^)]
-- 
-- さて、map関数を使うという方針はそのままに、
-- ($)と(.)を使ってこの式からラムダ式を消し去りましょう。

funks :: [Int -> Int -> Int]
funks = [(-), (div), (^),(\x y -> x*10+y)]

main = do
  print $ map (\f -> 2 `f` 3) funks
  print $ map (($3) . ($2)) funks
  print $ zipWith ($) (zipWith ($) funks (repeat 2)) (repeat 3)