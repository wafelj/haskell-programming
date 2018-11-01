module WrapItUp where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

import Data.Functor.Identity

embedded :: MaybeT
            (ExceptT String
                     (ReaderT () Identity))
            Int
embedded = MaybeT $ ExceptT $ ReaderT $ return <$> (const (Right (Just 1)))

embedded' :: MaybeT Identity Int
embedded' = MaybeT $ Identity $ Just 1

embedded'' :: MaybeT (ExceptT String 
                              Identity)
              Int
embedded'' = MaybeT $ ExceptT $ Identity (Right (Just 1))
