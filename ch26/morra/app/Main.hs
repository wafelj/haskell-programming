module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad (replicateM_)
import System.Random
import Data.Char (digitToInt)
import Text.Read (readMaybe)

data MorraState =
  MorraState {
    stateHistory :: History
  , stateScores  :: Scores
  }

type History = [Int]

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

gameRound :: StateT MorraState IO ()
gameRound = do
  lift $ putStr "Enter 1 or 2: "

  playerFingers <- lift $ getPlayerFingers
  aiFingers <- getAIFingers

  addToHistory playerFingers

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

getAIFingers :: StateT MorraState IO Int
getAIFingers = do
  state <- get
  let history = stateHistory state
  let lastTwo = take 2 history
  if length lastTwo == 2
    then lift $ predictNext history lastTwo
    else lift getRandomFingers

predictNext :: History -> [Int] -> IO Int
predictNext      []  _ = getRandomFingers
predictNext   (_:[]) _ = getRandomFingers
predictNext (_:_:[]) _ = getRandomFingers
predictNext (h1:h2:h3:hs) [l1, l2]
  | (h2, h3) == (l1, l2) = return h1
  | otherwise = predictNext (h2:h3:hs) [l1, l2]

getRandomFingers :: IO Int
getRandomFingers = getStdRandom $ randomR (1,2)

addToHistory :: Int -> StateT MorraState IO ()
addToHistory n = modify $ \s ->
  MorraState
    (n : (stateHistory s))
    (stateScores s)

incrementScorePlayer :: StateT MorraState IO ()
incrementScorePlayer = modify $ \s ->
  let scores = stateScores s in
  MorraState
    (stateHistory s)
    (Scores
      (scorePlayer scores + 1)
      (scoreAI scores))

incrementScoreAI :: StateT MorraState IO ()
incrementScoreAI = modify $ \s ->
  let scores = stateScores s in
  MorraState
    (stateHistory s)
    (Scores
      (scorePlayer scores)
      (scoreAI scores + 1))

game :: StateT MorraState IO ()
game = do
  lift $ putStr "How many rounds? "
  numRounds <- lift getNumRounds
  replicateM_ numRounds gameRound

  state <- get
  lift $ putStrLn $ congratsMessage $ stateScores state
  lift $ putStrLn $ show $ stateScores state

getNumRounds :: IO Int
getNumRounds = do
  s <- getLine
  let mn = readMaybe s :: Maybe Int
  case mn of
    Nothing -> getNumRounds
    Just n  -> return n

congratsMessage :: Scores -> String
congratsMessage scores = 
  "Victory goes to " ++ 
    (if scorePlayer scores > scoreAI scores 
      then "you" else "AI") ++ 
    "!"

main :: IO ()
main = do
  putStrLn "Welcome to Morra. You play odd."
  runStateT game $ MorraState [] $ Scores 0 0
  return ()
