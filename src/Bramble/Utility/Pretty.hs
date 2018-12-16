module Bramble.Utility.Pretty where

import Data.Functor (fmap)
import Data.Function ((.))
import Data.Text (Text, unlines)

class Pretty a where
  pretty :: a -> Text

instance Pretty a => Pretty [a] where
  pretty = unlines . fmap pretty
