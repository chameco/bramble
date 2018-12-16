module Bramble.Frontend.Vernacular where

-- TODO for this evening:
-- parameteric polymorphism for ADTs
-- parser

import Data.Eq (Eq)
import Data.Text (Text)

import Text.Show (Show)

import Bramble.Core.ADT

data Expression where
  Comment :: Text -> Expression
  The :: Expression -> Expression -> Expression
  Type :: Expression
  Var :: Text -> Expression
  Call :: Expression -> [Expression] -> Expression
deriving instance Show Expression
deriving instance Eq Expression

data Statement where
  Documentation :: Text -> Statement
  Define :: Text -> Expression -> Expression -> Statement
  Data :: Text -> Sum Expression -> Statement
deriving instance Show Statement
deriving instance Eq Statement
