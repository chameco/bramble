module Bramble.Frontend.Parser
  ( Bramble.Frontend.Parser.parse
  ) where

import Control.Applicative (pure, (*>), (<*))
import Control.Exception.Safe (MonadThrow, throw)

import Data.Void (Void)
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

import Bramble.Utility.Error
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

parse :: MonadThrow m => Text -> Text -> m [SExp]
parse file d = case Text.Megaparsec.parse (some sexp) (unpack file) d of
  Left err -> throw . ParseError . pack $ parseErrorPretty err
  Right x -> pure x
