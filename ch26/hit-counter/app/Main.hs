{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
    configCounts :: IORef (M.Map Text Integer)
  , configPrefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)
type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m = (newMap, count) where
                  newMap = M.insertWith (+) k 1 m
                  count  = newMap M.! k

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    prefix <- lift $ asks configPrefix
    let key' = mappend prefix unprefixed
    countsIORef <- lift $ asks configCounts
    newInteger <- lift $ lift $ updateCountsAndGetNewCount countsIORef key'
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInteger
              , "</h1>"
              ]

updateCountsAndGetNewCount :: IORef (M.Map Text Integer) -> Text -> IO Integer
updateCountsAndGetNewCount ioRef k = do
  m <- readIORef ioRef
  let (m', count) = bumpBoomp k m
  writeIORef ioRef m'
  return count

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter $ TL.pack prefixArg
      runR m = runReaderT m config
  scottyT 3000 runR app
