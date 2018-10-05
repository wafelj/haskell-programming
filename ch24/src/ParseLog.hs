{-# LANGUAGE QuasiQuotes #-}
module ParseLog where

import Text.Trifecta
import Data.Time
import Control.Applicative
import Text.RawString.QQ
import Text.Parser.LookAhead
import qualified Data.Map.Strict as Map
import Data.Char (isSpace)
import Test.QuickCheck

type Activity = String

type ActivityTimeMap = Map.Map Activity DiffTime

data Log = Log [DayLog]

instance Show Log where
  show (Log dayLogs) = 
    unlines (map show dayLogs)

instance Arbitrary Log where
  arbitrary = do
    dayLogs <- flexList
    return $ Log dayLogs

data DayLog =
  DayLog Day [TimeLog]

instance Show DayLog where
  show (DayLog day timeLogs) = 
    unlines $ dayString : timeStrings
      where dayString   = "# " ++ show day
            timeStrings = map show timeLogs

instance Arbitrary DayLog where
  arbitrary = do
    day <- arbitrary
    timeLogs <- flexList
    return $ DayLog (ModifiedJulianDay day) timeLogs

flexList :: Arbitrary a => Gen [a]
flexList = sized $ \n ->
  frequency
    [ (1, return [])
    , (n, (:) <$> arbitrary <*> flexList)
    ]

data TimeLog = 
  TimeLog { time :: TimeOfDay, activity :: Activity }

instance Show TimeLog where
  show (TimeLog t a) = show t ++ " " ++ a

instance Arbitrary TimeLog where
  arbitrary = do
    hour <- choose (0, 23)
    min  <- choose (0, 59)
    act  <- arbitrary
    return $ TimeLog (TimeOfDay hour min 0) act

parseLog :: Parser Log
parseLog = do
  whiteSpaceOrComments
  Log <$> manyTill parseDayLog eof

comment :: Parser ()
comment = do
  string "--"
  tokenSkipComments $ manyTill anyChar newline
  return ()

whiteSpaceOrComments :: Parser ()
whiteSpaceOrComments = whiteSpace >> skipMany comment

tokenSkipComments :: Parser a -> Parser a
tokenSkipComments p = do
  v <- p
  whiteSpaceOrComments
  return v

parseDayLog :: Parser DayLog
parseDayLog = do
  day <- parseDay
  timeLogs <- manyTill (tokenSkipComments parseTimeLog) (lookAhead hashOrEOF)
  return $ DayLog day timeLogs

hashOrEOF :: Parser ()
hashOrEOF = do
  (char '#' >> return ()) <|> eof

dayFormat :: String
dayFormat = "%F"

parseDay :: Parser Day
parseDay = do
  string "# "
  s <- tokenSkipComments (some (digit <|> char '-'))
  return $ (parseTimeOrErrorDefault
              dayFormat s :: Day)

parseTimeLog :: Parser TimeLog
parseTimeLog = do
  time <- parseTime'
  act  <- parseActivity
  return $ TimeLog time act

timeFormat :: String
timeFormat = "%H:%M"

parseTime' :: Parser TimeOfDay
parseTime' = do
  s <- tokenSkipComments (some (digit <|> char ':'))
  return $ (parseTimeOrErrorDefault
              timeFormat s :: TimeOfDay)

parseTimeOrErrorDefault :: ParseTime t => String -> String -> t
parseTimeOrErrorDefault =
  parseTimeOrError True defaultTimeLocale

parseActivity :: Parser String
parseActivity = do
  s <- manyTill anyChar newlineOrComment
  return $ trim s

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

newlineOrComment :: Parser ()
newlineOrComment = do
  (newline >> return ()) <|> comment

getActivityTimeTotal :: Parser ActivityTimeMap
getActivityTimeTotal = do
  (Log dayLogs) <- parseLog
  return $ makeActivityTimeMap dayLogs

getActivityTimeAvg :: Parser ActivityTimeMap
getActivityTimeAvg = do
  (Log dayLogs) <- parseLog
  return $ makeActivityTimeAvgMap
             (makeActivityTimeMap dayLogs)
             (length dayLogs)

makeActivityTimeAvgMap :: ActivityTimeMap -> Int -> ActivityTimeMap
makeActivityTimeAvgMap m dayCount =
  fmap (/ (fromInteger $ toInteger dayCount)) m

makeActivityTimeMap :: [DayLog] -> ActivityTimeMap
makeActivityTimeMap = foldr accTimeSum (Map.empty)
    where accTimeSum (DayLog day timeLogs) m = addActivitiesToMap timeLogs m

addActivitiesToMap :: [TimeLog] -> ActivityTimeMap -> ActivityTimeMap
addActivitiesToMap []     m = m
addActivitiesToMap (t:[]) m =
  Map.alter (alterMapFunction duration) (activity t) m
    where duration = diffTimeOfDay (time t) (dayFractionToTimeOfDay 1)
addActivitiesToMap (t1:t2:ts) m = 
  Map.alter (alterMapFunction duration) (activity t1) m'
    where duration = diffTimeOfDay (time t1) (time t2)
          m'       = addActivitiesToMap (t2:ts) m

alterMapFunction :: DiffTime -> Maybe DiffTime -> Maybe DiffTime
alterMapFunction duration Nothing  = Just duration
alterMapFunction duration (Just v) = Just $ v + duration

diffTimeOfDay :: TimeOfDay -> TimeOfDay -> DiffTime
diffTimeOfDay t1 t2 = timeOfDayToTime t2 - timeOfDayToTime t1

testLog :: String
testLog = [r|
-- wheee a comment

--anoth

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]
