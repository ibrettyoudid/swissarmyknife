{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiWayIf #-}
{- HLINT ignore "Eta reduce" -}

module Colour where
import NumberParsers

import Data.Word
import Text.Parsec
import Control.Monad.Identity

--escape = char '\27'
{-}
colourStr = do
   escape
   char '['
   i <- sepBy int (char ';')
   char 'm'
newtype ColourString = ColourString [(Colour, Char)]

instance Show ColourString where
   show (ColourString s) = showColour s -- dont try and show colour 255, it wont work
-}

readColour s = case runParser colourStr1 normal "readColour" s of
   Left l -> error $ show l
   Right r -> r

colourStr1 = choice [try $ do 
   string "\27["
   is <- sepBy colourStr2 (char ';')
   char 'm'
   colour <- getState
   str <- many $ noneOf "\27"
   str2 <- colourStr1

   return $ colourStr colour str ++ str2, try $ do

   colour <- getState
   str <- many1 $ noneOf "\27"
   str2 <- colourStr1
   return $ colourStr colour str ++ str2, do
      
   eof
   return []]

data Colour = Colour { fg :: Int, bg :: Int, bold :: Bool, under :: Bool, blink :: Bool, inver :: Bool, invis :: Bool } deriving (Eq, Ord, Show)

normal = Colour { fg = 7, bg = 0, bold = False, under = False, blink = False, inver = False, invis = False }

colourStr2 :: ParsecT String Colour Identity ()
colourStr2 = do
   colour <- getState
   i <- fromIntegral <$> integer
   if | i == 0             -> setState normal
      | i == 1             -> setState colour { bold  = True }
      | i == 4             -> setState colour { under = True }
      | i == 5             -> setState colour { blink = True }
      | i == 7             -> setState colour { inver = True }
      | i == 8             -> setState colour { invis = True }
      | i == 22            -> setState colour { bold  = False }
      | i == 24            -> setState colour { under = False }
      | i == 25            -> setState colour { blink = False }
      | i == 27            -> setState colour { inver = False }
      | i == 28            -> setState colour { invis = False }
      | i >= 30 && i <= 37 -> setState colour { fg = i-30 }
      | i >= 40 && i <= 47 -> setState colour { bg = i-40 }
      | i == 39            -> setState colour { fg = 4 }
      | i == 49            -> setState colour { bg = 0 }
      | otherwise          -> error $ show i

showColour s = showColour1 normal { fg = -1 } s

flag n f = if f then n else 0

showColour1 _          [              ] = []
showColour1 lastColour ((colour, c):xs) =
   (if colour /= lastColour then (("\27[" ++ 
         show (30 + fg colour) ++ ";" ++ 
         show (40 + bg colour) ++ ";" ++
         show (1  + flag 21 (bold  colour)) ++ ";" ++
         show (4  + flag 20 (under colour)) ++ ";" ++
         show (5  + flag 20 (blink colour)) ++ ";" ++
         show (7  + flag 20 (inver colour)) ++ ";" ++
         show (8  + flag 20 (invis colour)) ++ "m")++) else id) $
            c : showColour1 colour xs

colourStr colour s = map (colour, ) s

recolourStr colour s = map (\(_, c) -> (colour, c)) s

decolourStr s = map snd s

