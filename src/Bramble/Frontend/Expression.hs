module Bramble.Frontend.Expression where

import Control.Arrow (second)

import Data.Foldable (foldl')
import Data.Functor (fmap, (<$>))
import Data.Function (flip, ($))
import Data.Eq (Eq)
import Data.Bool (Bool)
import Data.Maybe (Maybe(..))
import Data.Text (Text)

import Text.Show (Show)

import Bramble.Core.Vernacular
import Bramble.Core.Calculus (Native(..))
import Bramble.Core.Named

data Expression where
  The :: Expression -> Expression -> Expression
  Type :: Expression
  Forall :: [(Text, Expression)] -> Expression -> Expression
  Var :: Text -> Expression
  Call :: Expression -> [Expression] -> Expression
  Fun :: [Text] -> Expression -> Expression
  Case :: Expression -> [(Text, Expression)] -> Expression
  RecordType :: Bool -> [(Text, Expression)] -> [Text] -> Expression
  Record :: [(Text, Expression)] -> Expression
  Field :: Expression -> Text -> Expression
  Foreign :: Native Expression -> Expression
deriving instance Show Expression
deriving instance Eq Expression

compileExpression :: Expression -> NameTerm
compileExpression (The ty t) = NameAnnotate (compileExpression t) $ compileExpression ty
compileExpression Type = NameStar
compileExpression (Forall [] b) = NamePi Nothing (NameFree "Unit") $ compileExpression b
compileExpression (Forall ns b) = foldl' (\b' (n, t) -> NamePi (Just n) (compileExpression t) b') (compileExpression b) ns
compileExpression (Var n) = NameFree n
compileExpression (Call f []) = NameApply (compileExpression f) $ NameFree "nil"
compileExpression (Call f args) = foldl' NameApply (compileExpression f) $ compileExpression <$> args
compileExpression (Fun [] b) = NameLambda Nothing (compileExpression b)
compileExpression (Fun ns b) = foldl' (flip NameLambda) (compileExpression b) $ Just <$> ns
compileExpression (Case x hs) = NameADTEliminate (compileExpression x) $ second compileExpression <$> hs
compileExpression (RecordType e fs es) = NameRow e (second compileExpression <$> fs) es
compileExpression (Record fs) = NameRowConstruct $ second compileExpression <$> fs
compileExpression (Field x fn) = NameRowEliminate (compileExpression x) fn
compileExpression (Foreign n) = NameForeign $ compileExpression <$> n

compileStatement :: Statement Expression -> Statement NameTerm
compileStatement = fmap compileExpression

compile :: [Statement Expression] -> [Statement NameTerm]
compile = fmap compileStatement 
