module WriteTheCode where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . (flip (-) 1)

rShow :: Show a
      => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
rPrintAndInc = ReaderT $ \a ->
                 do putStrLn $ "Hi: " ++ show a
                    return $ a + 1

sPrintIncAccum :: (Num a, Show a)
               => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
                   putStrLn $ "Hi :" ++ show s
                   return (show s, s + 1)
