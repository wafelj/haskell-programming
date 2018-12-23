{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import System.Exit
import qualified Data.ByteString.Lazy as B
import Data.Vector hiding (mapM_)

import Database.SQLite.Simple

import Data.Csv

import User

insertUser :: Query
insertUser =
  "INSERT OR REPLACE INTO users\
  \ VALUES (?, ?, ?, ?, ?, ?)"

insertRows :: Vector User -> IO ()
insertRows rows = do
  conn <- open "finger.db"
  mapM_ (execute conn insertUser) rows
  close conn

main :: IO ()
main = do
  args <- getArgs
  
  path <- case args of
    path:[] -> return path
    _       -> die "Usage: importusers FILENAME"

  rawData <- B.readFile path
  let parseResult = decode NoHeader rawData

  case parseResult of
    Left err -> die err
    Right rows -> insertRows rows
