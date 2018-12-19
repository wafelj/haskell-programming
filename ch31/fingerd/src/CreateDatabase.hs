{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import Data.Text (Text)
import Text.RawString.QQ
import Database.SQLite.Simple
       hiding (close, bind)
import qualified Database.SQLite.Simple
       as SQLite
import Database.SQLite.Simple.Types

import User

type UserRow =
  (Null, Text, Text, Text, Text, Text)

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
 |]

insertUser :: Query
insertUser =
  "INSERT INTO users\
  \ VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers =
  "SELECT * from users"

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where meRow :: UserRow
        meRow =
          (Null, "callen", "/bin/zsh",
           "/home/callen", "Chris Allen",
           "555-123-4567")

main :: IO ()
main = createDatabase
