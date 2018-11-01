module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad (replicateM_)
import System.Random
import Data.Char (digitToInt)
import Text.Read (readMaybe)

data Scores = 
  Scores {
    scorePlayer :: Int
  , scoreAI     :: Int
  }

instance Show Scores where
  show s = 
    "Score:" ++ 
    "\n  Player: " ++ show (scorePlayer s) ++ 
    "\n  AI: " ++ show (scoreAI s)

gameRound :: StateT Scores IO ()
gameRound = do
  lift $ putStr "Enter 1 or 2: "

  playerFingers <- lift $ getPlayerFingers
  aiFingers <- lift getAIFingers

  if odd (playerFingers + aiFingers)
    then incrementScorePlayer >> (lift $ putStrLn " -- You win!")
    else incrementScoreAI >> (lift $ putStrLn " -- AI wins.")

getPlayerFingers :: IO Int
getPlayerFingers = do
  c <- getChar

  if c `elem` "12"
    then do
      let mf = readMaybe [c] 
      case mf of
        Nothing -> getPlayerFingers
        Just f  -> return f
    else getPlayerFingers

getAIFingers :: IO Int
getAIFingers = getStdRandom $ randomR (1,2)

incrementScorePlayer :: StateT Scores IO ()
incrementScorePlayer = modify $ \s -> Scores 
  (scorePlayer s + 1) 
  (scoreAI s)

incrementScoreAI :: StateT Scores IO ()
incrementScoreAI = modify $ \s -> Scores 
  (scorePlayer s) 
  (scoreAI s + 1)

game :: StateT Scores IO ()
game = do
  lift $ putStr "How many rounds? "
  numRounds <- lift getNumRounds
  replicateM_ numRounds gameRound

  scores <- get
  lift $ putStrLn $ exitMessage scores
  lift $ putStrLn $ show scores

getNumRounds :: IO Int
getNumRounds = do
  s <- getLine
  let mn = readMaybe s :: Maybe Int
  case mn of
    Nothing -> getNumRounds
    Just n  -> return n

exitMessage :: Scores -> String
exitMessage scores = 
  "Victory goes to " ++ 
    (if scorePlayer scores > scoreAI scores 
      then "you" else "AI") ++ 
    "!"

main :: IO ()
main = do
  putStrLn "Welcome to Morra. You play odd."
  runStateT game $ Scores 0 0
  return ()
