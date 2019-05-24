module Bramble.Core.Vernacular where

import Data.Monoid (mconcat)
import Data.Functor (Functor, (<$>))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Eq (Eq)
import Data.Text (Text, intercalate)

import Text.Show (Show)

import Bramble.Utility.Pretty
import Bramble.Core.Inductive

data Statement a where
  Define :: Text -> a -> a -> Statement a
  Data :: Text -> [(Text, a)] -> Sum a -> Statement a
  Debug :: a -> Statement a
  Print :: a -> Statement a
  Infer :: a -> Statement a
  Check :: a -> a -> Statement a
  Env :: Statement a
deriving instance Show a => Show (Statement a)
deriving instance Eq a => Eq (Statement a)
deriving instance Functor Statement
deriving instance Foldable Statement
deriving instance Traversable Statement

instance Pretty a => Pretty (Statement a) where
  pretty (Define n t b) = mconcat ["Define ", n, " : ", pretty t, " := ", pretty b, "."]
  pretty (Data n ps s) = mconcat ["Data ", n, "(", intercalate ", " (prettyBinder <$> ps), ")", " := ", pretty s, "."]
  pretty (Debug s) = mconcat ["Debug ", pretty s, "."]
  pretty (Print s) = mconcat ["Print ", pretty s, "."]
  pretty (Infer s) = mconcat ["Infer ", pretty s, "."]
  pretty (Check s t) = mconcat ["Check ", pretty s, " : ", pretty t, "."]
  pretty Env = "Env."
