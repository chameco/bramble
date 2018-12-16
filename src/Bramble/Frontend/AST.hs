module Bramble.Frontend.AST where

import Control.Monad (mapM)
import Control.Applicative (pure, (<*>))
import Control.Exception.Safe (MonadThrow, throwString)

import Data.Monoid (mconcat)
import Data.Functor ((<$>))
import Data.Function (($), (.))
import Data.Eq (Eq)
import Data.Text (Text, unpack, unwords)

import Text.Show (Show)

import Bramble.Core.ADT
import Bramble.Frontend.Vernacular

data SExp where
  Symbol :: Text -> SExp
  List :: [SExp] -> SExp
deriving instance Show SExp
deriving instance Eq SExp

prettySExp :: SExp -> Text
prettySExp (Symbol n) = n
prettySExp (List exps) = unwords $ prettySExp <$> exps

vernacularizeExpression :: MonadThrow m => SExp -> m Expression
vernacularizeExpression (List (Symbol "#":exps)) = pure . Comment . unwords $ prettySExp <$> exps 
vernacularizeExpression (List [Symbol "the", ty, t]) = The <$> vernacularizeExpression ty <*> vernacularizeExpression t
vernacularizeExpression (List (exp:exps)) = Call <$> vernacularizeExpression exp <*> mapM vernacularizeExpression exps
vernacularizeExpression (Symbol "Type") = pure Type
vernacularizeExpression (Symbol n) = pure $ Var n
vernacularizeExpression exp = throwString . unpack $ mconcat ["Failed to convert S-expression \"", prettySExp exp, "\" to a vernacular expression"]

vernacularizeProduct :: MonadThrow m => SExp -> m (Product Expression)
vernacularizeProduct (List (Symbol n:exps)) = Product n <$> mapM vernacularizeExpression exps
vernacularizeProduct (Symbol n) = pure $ Product n []
vernacularizeProduct exp = throwString . unpack $ mconcat ["Failed to convert S-expression \"", prettySExp exp, "\" to a vernacular constructor definition"]

vernacularizeSum :: MonadThrow m => [SExp] -> m (Sum Expression)
vernacularizeSum exps = Sum <$> mapM vernacularizeProduct exps

vernacularizeStatement :: MonadThrow m => SExp -> m Statement
vernacularizeStatement (List (Symbol "#":exps)) = pure . Documentation . unwords $ prettySExp <$> exps 
vernacularizeStatement (List [Symbol "define", Symbol n, ty, t]) = Define n <$> vernacularizeExpression ty <*> vernacularizeExpression t
vernacularizeStatement (List (Symbol "data":Symbol n:exps)) = Data n <$> vernacularizeSum exps
vernacularizeStatement exp = throwString . unpack $ mconcat ["Failed to convert S-expression \"", prettySExp exp, "\" to a vernacular statement"]
