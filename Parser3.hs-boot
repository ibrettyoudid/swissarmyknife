module Parser3 where

import Parser3Types

fp :: RuleR Char a -> a -> Maybe [Char]
