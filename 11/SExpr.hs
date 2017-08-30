{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char
------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = ((\x xs -> x:xs) <$> p <*> (zeroOrMore p)) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (\x xs -> x:xs) <$> p <*> ((zeroOrMore p) <|> pure [])

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (\x y-> x:y) <$> (satisfy isAlpha) <*> (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseSExpr :: Parser SExpr
parseSExpr = parseAtom <|> parseSList
  where parseAtom = ((\x-> A $ I x) <$> (spaces *> ident <* spaces)) <|> ((\x-> A $ N x) <$> posInt)
        parseSList = (\x-> Comb x) <$> (spaces *> char '(' *>zeroOrMore parseSExpr <* char ')' <* spaces)
