import System.IO
import Favs

main = do 
   c <- getContents
   putStr $ replace ";" "\n" c
   
          
