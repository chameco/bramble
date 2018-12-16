module Bramble.Core.ADT where

import Prelude

import Data.Kind
import Data.Text (Text)

data Product ty = Product Text [ty]
             deriving (Show, Eq)
newtype Sum ty = Sum [Product ty]
            deriving (Show, Eq)

lookupProduct :: forall (ty :: Type). Text -> Sum ty -> Maybe (Product ty)
lookupProduct n (Sum ps) = go ps
  where go :: [Product ty] -> Maybe (Product ty)
        go [] = Nothing
        go (p@(Product n' _):r) | n == n' = Just p
                                | otherwise = go r
