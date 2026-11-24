module Consciousness where

import System.Random

consciousOf s = do
   let s1 = "I am conscious that " ++ s
   putStrLn s1
   consciousOf s1

main = do
   g < - initStdGen
   let (x, g) = uniformR (0,100) g
   consciousOf $ "x = "++show x