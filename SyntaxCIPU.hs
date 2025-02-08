-- Copyright 2025 Brett Curtis
module SyntaxCIPU where

data Iso alpha beta
  = Iso (alpha -> Maybe beta) (beta -> Maybe alpha)

