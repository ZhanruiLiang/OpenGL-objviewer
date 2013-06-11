module Utils where
import System.IO

debug s = do
  putStrLn.show$ s 
  hFlush stdout
