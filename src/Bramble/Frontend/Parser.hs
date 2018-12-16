module Bramble.Frontend.Parser
  ( parseSExp
  ) where

import Control.Applicative (pure, (*>), (<*))
import Control.Exception.Safe (MonadThrow, throwString)

import Data.Void (Void)
import Data.Monoid (mconcat)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Either (Either(..))
import Data.Bool (not, (||))
import Data.Char(Char, isSpace)
import Data.List (elem)
import Data.String (String)
import Data.Text (Text, pack, unpack)

import Text.Megaparsec (Parsec, parse, parseErrorPretty, some, many, (<|>))
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

parseSExp :: MonadThrow m => Text -> Text -> m SExp
parseSExp file d = case parse sexp (unpack file) d of
  Left err -> throwString $ mconcat ["Parse error: ", parseErrorPretty err]
  Right x -> pure x
