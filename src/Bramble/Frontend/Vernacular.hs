module Bramble.Frontend.Vernacular where

import Data.Foldable (foldr)
import Data.Functor ((<$>))
import Data.Function (flip, ($))
import Data.Eq (Eq)
import Data.Text (Text)

import Text.Show (Show)

import Bramble.Core.ADT
import Bramble.Core.Renamer

data Expression where
  The :: Expression -> Expression -> Expression
  Type :: Expression
  Forall :: [(Text, Expression)] -> Expression -> Expression
  Var :: Text -> Expression
  Call :: Expression -> [Expression] -> Expression
  Fun :: [Text] -> Expression -> Expression
deriving instance Show Expression
deriving instance Eq Expression

data Statement where
  Define :: Text -> Expression -> Expression -> Statement
  Data :: Text -> Sum Expression -> Statement
deriving instance Show Statement
deriving instance Eq Statement

compile :: Expression -> NameTerm
compile (The ty t) = NameAnnotate (compile t) $ compile ty
compile Type = NameStar
compile (Forall ns b) = foldr (\(n, t) b' -> NamePi n (compile t) b') (compile b) ns
compile (Var n) = NameFree n
compile (Call f args) = foldr (flip NameApply) (compile f) $ compile <$> args
compile (Fun ns b) = foldr NameLambda (compile b) ns
