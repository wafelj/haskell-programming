import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let line2 = filter (not . isSpace) (map toLower line1) in
    case (line2 == reverse line2) of
      True -> putStrLn "It's a palindrome!"
      False -> do
        putStrLn "Nope!"
        exitSuccess

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter name: "
  name <- getLine
  putStrLn "Enter age: "
  ageString <- getLine
  
  case mkPerson name (read ageString) of
    (Right person) -> putStrLn ("Yay! " ++ show person)
    (Left personInvalid) -> putStrLn ("Invalid: " ++ show personInvalid)
