module Bramble.Core.Vernacular where

import Data.Monoid (mconcat)
import Data.Functor (Functor)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Eq (Eq)
import Data.Text (Text)

import Text.Show (Show)

import Bramble.Utility.Pretty
import Bramble.Core.ADT

data Statement a where
  Define :: Text -> a -> a -> Statement a
  Data :: Text -> Sum a -> Statement a
  Debug :: a -> Statement a
  Check :: a -> Statement a
deriving instance Show a => Show (Statement a)
deriving instance Eq a => Eq (Statement a)
deriving instance Functor Statement
deriving instance Foldable Statement
deriving instance Traversable Statement

instance Pretty a => Pretty (Statement a) where
  pretty (Define n t b) = mconcat ["Define ", n, " : ", pretty t, " := ", pretty b, "."]
  pretty (Data n s) = mconcat ["Data ", n, " := ", pretty s, "."]
  pretty (Debug s) = mconcat ["Debug ", pretty s, "."]
  pretty (Check s) = mconcat ["Check ", pretty s, "."]
