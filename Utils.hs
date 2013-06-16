module Utils where
import System.IO
import Data.List

debug :: Show a => a -> IO ()
debug s = do
  putStrLn.show$ s 
  hFlush stdout

ppshow :: Show a => [a] -> String
ppshow l = "[" ++ concat (map ("\n"++) . map show $ l) ++ "]"
