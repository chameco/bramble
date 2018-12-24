module Bramble.Utility.Pretty where

import Data.Functor (fmap)
import Data.Monoid (mconcat)
import Data.Function ((.))
import Data.Text (Text, unlines)

class Pretty a where
  pretty :: a -> Text

instance Pretty a => Pretty [a] where
  pretty = unlines . fmap pretty

prettyBinder :: Pretty a => (Text, a) -> Text
prettyBinder (n, x) = mconcat [n, " : ", pretty x]
