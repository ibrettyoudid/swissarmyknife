{-# LANGUAGE LexicalNegation #-}

module Colour where

import Text.Parsec

escape = char '\27'

colourStr = do
   escape
   char '['
   i <- sepBy int (char ';')
   char 'm'

type ColourString = [(Char, Char8, Char8)]

instance Show ColourString where
   show s = showColour s -1 -1

showColour [              ] _      _      = ""
showColour ((c, fg, bg):xs) lastfg lastbg =
    (if fg /= lastfg then ('\27[' ++ (show 30 + fg) ++ "m":) else id) $
    (if bg /= lastbg then ('\27[' ++ (show 40 + bg) ++ "m":) else id)
    showColour xs fg bg