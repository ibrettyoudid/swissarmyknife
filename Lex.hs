{-# LANGUAGE NoMonomorphismRestriction #-}

module Lex where

import Data.Char
import Text.Parsec


data Tok str = Tok { tex :: str, p :: SourcePos, comb :: str, coma :: str }

lexemes = many lexeme

lexeme = do
   comb1 <- commentBefore
   p1 <- getPosition
   tex1 <- number <|> identifier
   coma1 <- lineComment
   return Tok { tex = tex1, p = p1, comb = comb1, coma = coma1 }

commentBefore = concat <$> many (blockComment <|> lineComment <|> whiteSpace)

myManyTill m end = manyTill (ifNotThen end m) end

ifNotThen n t = do
   m <- optionMaybe n
   case m of
      Just j -> fail "ifNotThen subparse succeeded"
      Nothing -> t

blockComment = do
   string "{-"
   manyTill anyChar $ string "-}"

lineComment = do
   string "--"
   manyTill anyChar crlf

whiteSpace = many (char ' ' <|> char '\n' <|> char '\r')

number = many $ satisfy isDigit

identifier = do
   h <-        satisfy (\c -> isAlpha c || c == '_')
   t <- many $ satisfy (\c -> isAlpha c || c == '_' || isDigit c)
   return $ h:t

punctuation = (: []) <$> satisfy (`elem` "()[]{},;")

operator = many $ satisfy (`elem` "!£$%^&*-=+~#:@<>\\/?")