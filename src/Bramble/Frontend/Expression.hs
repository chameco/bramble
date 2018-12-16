module Bramble.Frontend.Expression where

import Data.Foldable (foldr)
import Data.Functor (fmap, (<$>))
import Data.Function (flip, ($))
import Data.Eq (Eq)
import Data.Maybe (Maybe(..))
import Data.Text (Text)

import Text.Show (Show)

import Bramble.Core.Renamer
import Bramble.Core.Vernacular

data Expression where
  The :: Expression -> Expression -> Expression
  Type :: Expression
  Forall :: [(Text, Expression)] -> Expression -> Expression
  Var :: Text -> Expression
  Call :: Expression -> [Expression] -> Expression
  Fun :: [Text] -> Expression -> Expression
deriving instance Show Expression
deriving instance Eq Expression

compileExpression :: Expression -> NameTerm
compileExpression (The ty t) = NameAnnotate (compileExpression t) $ compileExpression ty
compileExpression Type = NameStar
compileExpression (Forall [] b) = NamePi Nothing (NameFree "Unit") $ compileExpression b
compileExpression (Forall ns b) = foldr (\(n, t) b' -> NamePi (Just n) (compileExpression t) b') (compileExpression b) ns
compileExpression (Var n) = NameFree n
compileExpression (Call f []) = NameApply (compileExpression f) $ NameFree "nil"
compileExpression (Call f args) = foldr (flip NameApply) (compileExpression f) $ compileExpression <$> args
compileExpression (Fun [] b) = NameLambda Nothing (compileExpression b)
compileExpression (Fun ns b) = foldr NameLambda (compileExpression b) $ Just <$> ns

compileStatement :: Statement Expression -> Statement NameTerm
compileStatement = fmap compileExpression

compile :: [Statement Expression] -> [Statement NameTerm]
compile = fmap compileStatement 
