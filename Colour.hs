{-# LANGUAGE LexicalNegation #-}

module Colour where

import Data.Word
--import Text.Parsec

--escape = char '\27'
{-}
colourStr = do
   escape
   char '['
   i <- sepBy int (char ';')
   char 'm'
-}
newtype ColourString = ColourString [(Word8, Word8, Char)]

instance Show ColourString where
   show (ColourString s) = showColour s -- dont try and show colour 255, it wont work

showColour s = showColour1 255 255 s

showColour1 _      _      [              ] = ""
showColour1 lastfg lastbg ((fg, bg, c):xs) =
    (if fg /= lastfg then (("\27[" ++ show (30 + fg) ++ "m")++) else id) $
    (if bg /= lastbg then (("\27[" ++ show (40 + bg) ++ "m")++) else id) $
    c : showColour1 fg bg xs

colourStr fg bg s = map (fg, bg, ) s

recolourStr fg bg s = map (\(_, _, c) -> (fg, bg, c)) s

fgrecolourStr fg s = map (\(_, bg, c) -> (fg, bg, c)) s

bgrecolourStr bg s = map (\(fg, _, c) -> (fg, bg, c)) s

decolourStr s = map (\(_, _, c) -> c) s