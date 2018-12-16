module Bramble.Frontend where

import Control.Applicative (pure)
import Control.Monad ((>=>))
import Control.Exception.Safe (MonadThrow)

import Data.Function ((.))
import Data.Text (Text)

import Bramble.Frontend.Parser
import Bramble.Frontend.AST
import Bramble.Frontend.Expression
import Bramble.Core.Renamer
import Bramble.Core.AST
import Bramble.Core.Vernacular

toCore :: MonadThrow m => Text -> Text -> m [Statement Term]
toCore file = parse file
              >=> vernacularize
              >=> pure . compile
              >=> rename
