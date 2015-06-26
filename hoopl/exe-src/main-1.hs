import Compiler.Hoopl
import IR

prog1 :: Graph Insn C C
prog1 = mkFirst (Entry ["a","b","c"])
        <*> mkMiddle (Assign "z" $ Triop FMA (Load "a") (Load "b") (Load "c"))
        <*> mkMiddle (Assign "y" $ Triop FMA (Load "a") (Load "b") (Load "z"))
        <*> mkMiddle (Assign "x" $ Binop Add (Load "z") (Load "y"))
        <*> mkLast (Return [Load "z"])


main :: IO ()
main = do
  putStrLn "main"
  putStrLn $ showGraph show prog1

  putStrLn "main -O1"
  putStrLn $ showGraph show $ body $ optimize $ Proc "aaaa" prog1

  print theLabel
