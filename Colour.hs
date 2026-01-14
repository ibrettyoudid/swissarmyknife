module Colour where

import Text.Parsec

escape = char '\27'

colourStr = do
    escape
    char '['
    i <- sepBy int (char ';')
    char 'm'