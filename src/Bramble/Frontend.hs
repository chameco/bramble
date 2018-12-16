module Bramble.Frontend where

import Control.Applicative (pure)
import Control.Monad ((>=>))
import Control.Exception.Safe (MonadThrow)

import Data.Function ((.))
import Data.Text (Text)

import Bramble.Frontend.Parser
import Bramble.Frontend.AST
import Bramble.Frontend.Vernacular
import Bramble.Core.Renamer
import Bramble.Core.AST

toCore :: MonadThrow m => Text -> m Term
toCore = parseSExp "input"
         >=> vernacularizeExpression
         >=> pure . compile
         >=> rename
