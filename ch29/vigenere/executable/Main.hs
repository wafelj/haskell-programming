import System.Environment (getArgs)
import System.Exit
import System.IO

import Control.Monad
import Data.Maybe
import Text.Read

import qualified Vigenere

data Mode = ModeEncrypt | ModeDecrypt

defaultTimeout :: Int
defaultTimeout = 10000

main :: IO ()
main = do
  args <- getArgs

  (mode, timeout, keyword) <- 
    case args of
      "-e":"-t":timeoutStr:keyword:[] -> do
        timeout <- parseTimeout timeoutStr
        return (ModeEncrypt, timeout, keyword)
      "-d":"-t":timeoutStr:keyword:[] -> do
        timeout <- parseTimeout timeoutStr
        return (ModeDecrypt, timeout, keyword)
      "-e":keyword:[] ->
        return (ModeEncrypt, defaultTimeout, keyword)
      "-d":keyword:[] ->
        return (ModeDecrypt, defaultTimeout, keyword)
      _ ->
        die "Usage: vigenere (-d|-e) [-t miliseconds] keyword"

  inputAvailable <- hWaitForInput stdin timeout
  unless inputAvailable $ 
    die $ "Timed out after " ++ show timeout ++ " ms."

  message <- getLine
  
  putStrLn $ 
    (case mode of
      ModeEncrypt -> Vigenere.encrypt
      ModeDecrypt -> Vigenere.decrypt
    ) keyword message

parseTimeout :: String -> IO Int
parseTimeout s = do
  let timeout = readMaybe s
  when (isNothing timeout) $ die "Invalid value for timeout"
  return $ fromJust timeout
