module Bramble.Core.ADT where

import Data.Kind (Type)
import Data.Monoid ((<>))
import Data.Functor (Functor, (<$>))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Eq (Eq, (==))
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Bool (otherwise)
import Data.List (null)

import Data.Text (Text, unwords, intercalate)

import Text.Show (Show)

import Bramble.Utility.Pretty

data Product ty = Product Text [ty]
             deriving (Show, Eq, Functor, Foldable, Traversable)
newtype Sum ty = Sum [Product ty]
            deriving (Show, Eq, Functor, Foldable, Traversable)

instance Pretty a => Pretty (Sum a) where
  pretty (Sum ps) = intercalate " | " $ (\(Product cn args) -> cn <> (if null args then "" else " ") <> unwords (pretty <$> args)) <$> ps

lookupProduct :: forall (ty :: Type). Text -> Sum ty -> Maybe (Product ty)
lookupProduct n (Sum ps) = go ps
  where go :: [Product ty] -> Maybe (Product ty)
        go [] = Nothing
        go (p@(Product n' _):r) | n == n' = Just p
                                | otherwise = go r
