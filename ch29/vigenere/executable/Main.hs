import System.Environment (getArgs)
import System.Exit

import qualified Vigenere

data Mode = ModeEncrypt | ModeDecrypt

main :: IO ()
main = do
  args <- getArgs

  (mode, keyword) <- 
    case args of
      "-e":keyword:_ -> return (ModeEncrypt, keyword)
      "-d":keyword:_ -> return (ModeDecrypt, keyword)
      _              -> die "Usage: vigenere (-d|-e)"

  message <- getLine
  
  putStrLn $ 
    (case mode of
      ModeEncrypt -> Vigenere.encrypt
      ModeDecrypt -> Vigenere.decrypt
    ) keyword message
