module Bramble.Core.Native where

import Data.Eq (Eq)
import Data.Int (Int)
import Data.Text (Text)

import Text.Show (Show)

data Native where
  NativeType :: Text -> Native
  NativeInt :: Int -> Native
  NativeString :: Text -> Native
  NativeFunction :: Text -> Native
deriving instance Show Native
deriving instance Eq Native
