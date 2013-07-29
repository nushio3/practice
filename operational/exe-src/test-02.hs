-- game of nim in operational.

{-# LANGUAGE GADTs #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Operational
import Control.Monad.State
import System.Random

-- | Player's language
data PLang a where
    ReadBoard :: PLang Board
    PlayMove  :: Int -> Int -> PLang Bool

type Player m a = ProgramT PLang m a
type Board = [Int]

readBoard :: ProgramT PLang m Board
readBoard = singleton ReadBoard

playMove :: Int -> Int -> ProgramT PLang m Bool
playMove = ((.).(.)) singleton PlayMove


initialGameState :: Board
initialGameState = [3,5,8]

runGame :: MonadIO m => Player m () -> Player m () -> m ()
runGame player1 player2 = go initialGameState player1 player2
  where
    go gameState p1 p2 = do
      p1v <- viewT p1
      case p1v of
        Return _ -> return ()
        ReadBoard :>>= kp1 -> go gameState (kp1 gameState) p2
        PlayMove place amount :>>= kp1 ->  go gameState p2 (kp1 True)

main :: IO ()
main = return ()
