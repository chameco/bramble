module Bramble.Frontend.Parser where

import Control.Applicative ((*>), (<*))

import Data.Void (Void)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Bool (not, (||))
import Data.Char(Char, isSpace)
import Data.List (elem)
import Data.String (String)
import Data.Text (Text, pack)

import Text.Megaparsec (Parsec, some, many, (<|>))
import Text.Megaparsec.Char (satisfy, char, spaceChar)

import Bramble.Frontend.AST

type Parser = Parsec Void Text

sexp :: Parser SExp
sexp = many spaceChar *>
  (Symbol . pack <$> some symchar
   <|> List <$> (char '(' *> many spaceChar *> many (many spaceChar *> sexp <* many spaceChar) <* char ')')
  ) <* many spaceChar
  where symchar :: Parser Char
        symchar = satisfy $ \c -> not (isSpace c || c `elem` special)
        special :: String
        special = "()"
