{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
       (decodeUtf8, encodeUtf8)
import Data.Typeable
import Network.Socket hiding (recv)
import Database.SQLite.Simple
       hiding (close, bind)
import qualified Database.SQLite.Simple
       as SQLite
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString
       (recv, sendAll)
import User

data DuplicateData =
  DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

allUsers :: Query
allUsers =
  "SELECT * from users"

getUserQuery :: Query
getUserQuery =
  "SELECT * from users where username = ?"

getUser :: Connection
        -> Text
        -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return $ Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

returnUsers :: Connection
            -> Socket
            -> IO ()
returnUsers conn soc = do
  rows <- query_ conn allUsers
  let usernames = map username rows
      newlineSeparated =
        T.concat $
        intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell
            homeDir realName _) = BS.concat
  ["Login: ", e username, "\t\t\t\t",
   "Name: ", e realName, "\n",
   "Directory: ", e homeDir, "\t\t\t",
   "Shell: ", e shell, "\n"]
  where e = encodeUtf8


returnUser :: Connection
           -> Socket
           -> Text
           -> IO ()
returnUser dbConn soc username = do
  maybeUser <-
    getUser dbConn (T.strip username)
  case maybeUser of 
    Nothing -> do
      putStrLn
        ("Couldn't find matching user\
          \ for username: "
        ++ (show username)) 
      return ()
    Just user ->
      sendAll soc (formatUser user)

handleQuery :: Connection
            -> Socket
            -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc 
    name ->
      returnUser dbConn soc
      (decodeUtf8 name)

handleQueries :: Connection
              -> Socket
              -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  close soc

main :: IO ()
main = withSocketsDo $ do
  addrinfos <- 
    getAddrInfo
    (Just (defaultHints
      {addrFlags = [AI_PASSIVE]}))
    Nothing (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr)
          Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  -- only one connection open at a time
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  close sock
