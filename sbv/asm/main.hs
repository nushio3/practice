import Control.Monad
import Data.List
import Data.SBV
import qualified Data.Map as Map
import Text.Printf

type SAddr = SWord8

newtype Val = X Int
  deriving (Eq, Ord)
instance Show Val where show (X x) = "x"++ show x
           
data Inst 
  = Imm Val Int
  | Add Val Val Val
  | Sub Val Val Val
  | Mul Val Val Val
  deriving (Eq, Ord, Show)    
           
slot :: Inst -> Int
slot (Imm _ _) = 0
slot (Add _ _ _) = 1
slot (Sub _ _ _) = 1
slot (Mul _ _ _) = 2
           
delay :: Inst -> Int
delay (Imm _ _) = 1
delay (Add _ _ _) = 1
delay (Sub _ _ _) = 1
delay (Mul _ _ _) = 2


rhs :: Inst -> [Val]
rhs (Imm x _) = [x]
rhs (Add x _ _) = [x]
rhs (Sub x _ _) = [x]
rhs (Mul x _ _) = [x]

lhs :: Inst -> [Val]
lhs (Imm _ _) = []
lhs (Add _ x y) = [x,y]
lhs (Sub _ x y) = [x,y]
lhs (Mul _ x y) = [x,y]


type Pgm = [Inst]
  

myPgm :: Pgm
myPgm = 
  [ Imm (X 1)  123
  , Imm (X 2)  456
  , Add (X 3)  (X 1) (X 2)
  , Sub (X 4)  (X 1) (X 2)
  , Mul (X 5)  (X 3) (X 4)
  , Add (X 6)  (X 4) (X 3)    
  , Mul (X 7)  (X 5) (X 6)
  ]
    
  
data Inst3 = Inst3   
  {lineName :: String
  ,lineSN :: SAddr
  ,lineInst :: Inst}

pprPgm :: String -> Pgm -> String
pprPgm solStr pgm0 = unlines [pprLine l | l <- [0..maxLine]]
  where
    pprLine :: Int -> String
    pprLine l = intercalate ";" [pprInst (l,n) | n <- [0..2]]
    
    pprInst :: (Int,Int) -> String
    pprInst k = printf "%-20s" $
      maybe "" show (Map.lookup k pgmMap )
    
    maxLine :: Int
    maxLine = maximum $ map fst $ Map.keys pgmMap
    
    lookupY :: Int -> Int
    lookupY i = read $ words str0 !!2
      where 
        str0 = head
          [str 
          | str <- lines solStr
          , words str!!0 == "l" ++ show i]
    
    pgm2 =  zip [i | i <-[0..]] pgm0
    
    pgmMap :: Map.Map (Int, Int) Inst
    pgmMap = Map.fromList
      [((lookupY i,slot inst),inst)  | (i, inst)<- pgm2]
    
pgmProblem :: Pgm -> SAddr -> Symbolic SBool
pgmProblem pgm0 maxAddr = do
  let lnPgm = zip ["l" ++ show i | i <-[0..]] pgm0
  lnPgm3 <- forM lnPgm $ \(ln,inst) -> do
    lnSym <- symbolic ln
    return $ Inst3 ln lnSym inst
    
  forM_ lnPgm3 $ \lin -> do
    forM_ (rhs $ lineInst lin) $ \rhsVal -> do
      sequence_ 
        [ constrain $ lineSN lin2 .>= lineSN lin + fromIntegral (delay (lineInst lin))
        | lin2 <- lnPgm3, rhsVal `elem` lhs (lineInst lin2)] 
  
  forM_ lnPgm3 $ \lin -> do
    forM_ lnPgm3 $ \lin2 -> do    
      when (lineName lin < lineName lin2 && 
            slot (lineInst lin) == slot (lineInst lin2)) $
        constrain (lineSN lin ./= lineSN lin2)
        

  return $ bAnd [lineSN lin .<= maxAddr | lin <- lnPgm3]

  
main :: IO ()  
main = do
  str <- search
  putStrLn $ pprPgm str myPgm
  where  
    search = unis 1
    
    isOk = isInfixOf "Satisfiable"
    
    unis n = do
      s <- test n
      if isOk s then bins (div n 2) n else unis (2*n)
    
    bins a b 
      | a >= b-1 = test b
      | otherwise = do
          let c = div (a+b) 2
          s <- test c
          if isOk s then bins a c else bins c b
      
    
    test :: Int -> IO String
    test n = fmap show $ sat $ pgmProblem myPgm (fromIntegral n)
