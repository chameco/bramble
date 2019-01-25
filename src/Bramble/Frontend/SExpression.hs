module Bramble.Frontend.SExpression where

import Control.Arrow (first, second)
import Control.Monad (mapM)
import Control.Applicative (pure, (<*>))
import Control.Exception.Safe (MonadThrow, throw)

import Data.Monoid ((<>))
import Data.Functor ((<$>))
import Data.Function (flip, ($), (.))
import Data.Eq (Eq)
import Data.Bool (Bool(..), otherwise)
import Data.List (filter)
import Data.Text (Text, unwords)

import Text.Show (Show)

import Bramble.Utility.Pretty
import Bramble.Utility.Error
import Bramble.Core.Inductive
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

readLambdaBinder :: MonadThrow m => SExp -> m Text
readLambdaBinder (Symbol n) = pure n
readLambdaBinder exp = throw . ReadLambdaBinderError $ pretty exp

readLambdaBinders :: MonadThrow m => SExp -> m [Text]
readLambdaBinders (List exps) = mapM readLambdaBinder exps
readLambdaBinders exp = throw . ReadLambdaBinderListError $ pretty exp

lambdaHelper :: MonadThrow m => SExp -> SExp -> m Expression
lambdaHelper ns b = Fun <$> readLambdaBinders ns <*> readExpression b

readBinder :: MonadThrow m => SExp -> m (Text, Expression)
readBinder (List [Symbol n, t]) = (n,) <$> readExpression t
readBinder exp = throw . ReadPiBinderError $ pretty exp

readBinders :: MonadThrow m => SExp -> m [(Text, Expression)]
readBinders (List exps) = mapM readBinder exps
readBinders exp = throw . ReadPiBinderListError $ pretty exp

piHelper :: MonadThrow m => SExp -> SExp -> m Expression
piHelper ns b = Forall <$> readBinders ns <*> readExpression b

readCase :: MonadThrow m => SExp -> m (Text, Expression)
readCase (List [Symbol n, x]) = (n,) <$> readExpression x
readCase exp = throw . ReadCaseError $ pretty exp

rowHelper :: forall m. MonadThrow m => Bool -> [SExp] -> m Expression
rowHelper e bs = do
  (fs, es) <- splitRowBinders bs
  pure $ RecordType e fs es
  where splitRowBinders :: [SExp] -> m ([(Text, Expression)], [Text])
        splitRowBinders [] = pure ([], [])
        splitRowBinders (List [Symbol "!", Symbol fn]:xs) = second (fn:) <$> splitRowBinders xs 
        splitRowBinders (List [Symbol fn, t]:xs) = do
          t' <- readExpression t
          first ((fn, t'):) <$> splitRowBinders xs 
        splitRowBinders exp = throw . ReadRowBinderError $ pretty exp

readField :: MonadThrow m => SExp -> m (Text, Expression)
readField (List [Symbol n, x]) = (n,) <$> readExpression x
readField exp = throw . ReadFieldError $ pretty exp

readExpression :: MonadThrow m => SExp -> m Expression
readExpression (List [Symbol "the", t, x]) = The <$> readExpression t <*> readExpression x
readExpression (List [Symbol "lambda", ns, b]) = lambdaHelper ns b
readExpression (List [Symbol "λ", ns, b]) = lambdaHelper ns b
readExpression (List [Symbol "pi", ns, b]) = piHelper ns b
readExpression (List [Symbol "Π", ns, b]) = piHelper ns b
readExpression (List [Symbol "∀", ns, b]) = piHelper ns b
readExpression (List [Symbol ".", x, Symbol fn]) = flip Field fn <$> readExpression x
readExpression (List (Symbol "case":x:hs)) = Case <$> readExpression x <*> mapM readCase hs
readExpression (List (Symbol "row":bs)) = rowHelper False bs
readExpression (List (Symbol "row...":bs)) = rowHelper True bs
readExpression (List (Symbol "record":fs)) = Record <$> mapM readField fs
readExpression (List (exp:exps)) = Call <$> readExpression exp <*> mapM readExpression exps
readExpression (Symbol "Type") = pure Type
readExpression (Symbol n) = pure $ Var n
readExpression exp = throw . ReadExpressionError $ pretty exp

readProduct :: MonadThrow m => SExp -> m (Product Expression)
readProduct (List (Symbol n:exps)) = Product n <$> mapM readExpression exps
readProduct (Symbol n) = pure $ Product n []
readProduct exp = throw . ReadConstructorError $ pretty exp

readSum :: MonadThrow m => [SExp] -> m (Sum Expression)
readSum exps = Sum <$> mapM readProduct exps

readStatement :: MonadThrow m => SExp -> m (Statement Expression)
readStatement (List [Symbol "define", Symbol n, ty, t]) = Define n <$> readExpression ty <*> readExpression t
readStatement (List (Symbol "data":Symbol n:ps:exps)) = Data n <$> readBinders ps <*> readSum exps
readStatement (List [Symbol "debug", x]) = Debug <$> readExpression x
readStatement (List [Symbol "infer", x]) = Infer <$> readExpression x
readStatement (List [Symbol "check", x, t]) = Check <$> readExpression x <*> readExpression t
readStatement (List [Symbol "env"]) = pure Env
readStatement exp = throw . ReadStatementError $ pretty exp

read :: MonadThrow m => [SExp] -> m [Statement Expression]
read = mapM readStatement
