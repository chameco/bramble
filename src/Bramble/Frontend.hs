module Bramble.Frontend where

import Control.Applicative (pure)
import Control.Monad ((>=>))
import Control.Exception.Safe (MonadThrow)

import Data.Function ((.))
import Data.Text (Text)

import Bramble.Frontend.Parser
import Bramble.Frontend.SExpression
import Bramble.Frontend.Expression
import Bramble.Core.Vernacular
import Bramble.Core.Named
import Bramble.Core.Calculus

frontend :: MonadThrow m => Text -> Text -> m [Statement Term]
frontend file = parse file
                >=> read
                >=> pure . compile
                >=> rename
