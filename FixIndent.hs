{- HLINT ignore "Redundant return" -}
module FixIndent where

import Shell
import Favs
import Data.List
import qualified Data.Map as M

targetIndentStep = 3

fixi = mapM (\fn -> do
   text <- readFile fn
   let
      mylines = lines text 
      mycounts = map (fromMaybe 0 . findIndex (/= ' ')) mylines
      mycounts1 = filter ((> (length mylines `div` 10)) . snd) 
         $ M.toList $ counts1 $ filter (>0) $ mycounts
      mycounts2 = map (\mod1 -> sums $ map (\(i, c) -> (i `mod` mod1, c)) mycounts1) [1..8]
      mycounts3 = map (\mod1 -> let 
         m = sums $ map (\(i, c) -> (i `mod` mod1 == 0, c)) mycounts1
         t = fromMaybe 0 $ M.lookup True m
         f = fromMaybe 0 $ M.lookup False m
         in ifJust (t * (mod1 - 1) > f) (t * mod1)) [1..8]
      currentIndentStep = (1 +) $ fromMaybe (-1) $ elemIndex (maximum mycounts3) mycounts3

   putStrLn fn
   print mycounts
   print mycounts2
   print mycounts3
   print currentIndentStep
   putStrLn ""
   let lines2 = zipWith 
         (\ci tx -> let
            cinew  = round (fromIntegral ci / fromIntegral currentIndentStep) * targetIndentStep
            cidiff = cinew - ci
            in if cidiff >= 0 
               then replicate cidiff ' ' ++ tx
               else drop (negate cidiff) tx) mycounts mylines
   writeFile fn $ unlines lines2) 
                  $ filter (ext $= ".hs") $ filePaths "/home/brett/swissarmyknife/"

