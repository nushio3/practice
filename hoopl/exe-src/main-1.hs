import Compiler.Hoopl
import IR

prog1 :: Graph Insn C C
prog1 = mkFirst (Entry ["a","b","c"])
        <*> mkMiddle (Assign "z" $ Triop FMA (Load "a") (Load "b") (Load "c"))
        <*> mkLast (Return [Load "z"])


main :: IO ()
main = do
  putStrLn $ showGraph show prog1
