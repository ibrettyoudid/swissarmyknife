import System.IO
import Data.List

main = do 
   c <- getContents
   putStr $ intercalate ";" $ lines c
   
          