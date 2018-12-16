module Bramble.Frontend.AST where

import Control.Monad (mapM)
import Control.Applicative (pure, (<*>))
import Control.Exception.Safe (MonadThrow, throw)

import Data.Monoid ((<>))
import Data.Functor ((<$>))
import Data.Function (($), (.))
import Data.Eq (Eq)
import Data.Bool (Bool(..), otherwise)
import Data.List (filter)
import Data.Text (Text, unwords)

import Text.Show (Show)

import Bramble.Utility.Pretty
import Bramble.Utility.Error
import Bramble.Core.ADT
import Bramble.Core.Vernacular
import Bramble.Frontend.Expression

data SExp where
  Symbol :: Text -> SExp
  List :: [SExp] -> SExp
deriving instance Show SExp
deriving instance Eq SExp

instance Pretty SExp where
  pretty (Symbol n) = n
  pretty (List exps) = "(" <> unwords (pretty <$> exps) <> ")"

isComment :: SExp -> Bool
isComment (List (Symbol "#":_)) = True
isComment _ = False

stripComments :: [SExp] -> [SExp]
stripComments [] = []
stripComments all@(x:xs)
  | isComment x = stripComments xs
  | otherwise = case all of
      List exps:xs' -> List (filter isComment exps):stripComments xs'
      _ -> x:stripComments xs

vernacularizeLambdaBinder :: MonadThrow m => SExp -> m Text
vernacularizeLambdaBinder (Symbol n) = pure n
vernacularizeLambdaBinder exp = throw . VernacularLambdaBinderError $ pretty exp

vernacularizeLambdaBinders :: MonadThrow m => SExp -> m [Text]
vernacularizeLambdaBinders (List exps) = mapM vernacularizeLambdaBinder exps
vernacularizeLambdaBinders exp = throw . VernacularLambdaBinderListError $ pretty exp

lambdaHelper :: MonadThrow m => SExp -> SExp -> m Expression
lambdaHelper ns b = Fun <$> vernacularizeLambdaBinders ns <*> vernacularizeExpression b

vernacularizePiBinder :: MonadThrow m => SExp -> m (Text, Expression)
vernacularizePiBinder (List [Symbol n, t]) = (n,) <$> vernacularizeExpression t
vernacularizePiBinder exp = throw . VernacularPiBinderError $ pretty exp

vernacularizePiBinders :: MonadThrow m => SExp -> m [(Text, Expression)]
vernacularizePiBinders (List exps) = mapM vernacularizePiBinder exps
vernacularizePiBinders exp = throw . VernacularPiBinderListError $ pretty exp

piHelper :: MonadThrow m => SExp -> SExp -> m Expression
piHelper ns b = Forall <$> vernacularizePiBinders ns <*> vernacularizeExpression b

vernacularizeExpression :: MonadThrow m => SExp -> m Expression
vernacularizeExpression (List [Symbol "the", ty, t]) = The <$> vernacularizeExpression ty <*> vernacularizeExpression t
vernacularizeExpression (List [Symbol "lambda", ns, b]) = lambdaHelper ns b
vernacularizeExpression (List [Symbol "λ", ns, b]) = lambdaHelper ns b
vernacularizeExpression (List [Symbol "pi", ns, b]) = piHelper ns b
vernacularizeExpression (List [Symbol "∀", ns, b]) = piHelper ns b
vernacularizeExpression (List (exp:exps)) = Call <$> vernacularizeExpression exp <*> mapM vernacularizeExpression exps
vernacularizeExpression (Symbol "Type") = pure Type
vernacularizeExpression (Symbol n) = pure $ Var n
vernacularizeExpression exp = throw . VernacularExpressionError $ pretty exp

vernacularizeProduct :: MonadThrow m => SExp -> m (Product Expression)
vernacularizeProduct (List (Symbol n:exps)) = Product n <$> mapM vernacularizeExpression exps
vernacularizeProduct (Symbol n) = pure $ Product n []
vernacularizeProduct exp = throw . VernacularConstructorError $ pretty exp

vernacularizeSum :: MonadThrow m => [SExp] -> m (Sum Expression)
vernacularizeSum exps = Sum <$> mapM vernacularizeProduct exps

vernacularizeStatement :: MonadThrow m => SExp -> m (Statement Expression)
vernacularizeStatement (List [Symbol "define", Symbol n, ty, t]) = Define n <$> vernacularizeExpression ty <*> vernacularizeExpression t
vernacularizeStatement (List (Symbol "data":Symbol n:exps)) = Data n <$> vernacularizeSum exps
vernacularizeStatement (List [Symbol "debug", x]) = Debug <$> vernacularizeExpression x
vernacularizeStatement (List [Symbol "check", x]) = Check <$> vernacularizeExpression x
vernacularizeStatement exp = throw . VernacularStatementError $ pretty exp

vernacularize :: MonadThrow m => [SExp] -> m [Statement Expression]
vernacularize = mapM vernacularizeStatement
