module Bramble.Backend.Interpreter where

import GHC.Err (error)

import Control.Applicative (pure, (*>))
import Control.Arrow (second)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Safe (MonadThrow)

import Data.Kind (Type)
import Data.Functor (fmap, void, (<$>), ($>))
import Data.Foldable (foldlM)
import Data.Function (($), (.))
import Data.Eq ((==))
import Data.Bool (otherwise)
import Data.Text.IO (putStrLn)

import Bramble.Utility.Pretty
import Bramble.Core.AST
import Bramble.Core.Vernacular

substNeutral :: Name -> Value -> Neutral -> Value
substNeutral n x (NFree n')
  | n == n' = x
  | otherwise = VNeutral $ NFree n'
substNeutral n x (NApply m y) = vApply (substNeutral n x m) $ substValue n x y
substNeutral n x (NADTEliminate m t hs) = vADTEliminate (substNeutral n x m) (substValue n x t) $ second (substValue n x) <$> hs

substValue :: Name -> Value -> Value -> Value
substValue n x (VLambda f) = VLambda $ f . substValue n x
substValue _ _ VStar = VStar
substValue n x (VPi t f) = VPi (substValue n x t) $ f . substValue n x
substValue n x (VNeutral m) = substNeutral n x m
substValue n x (VADT n' s) = VADT n' $ substValue n x <$> s
substValue n x (VADTConstruct cn t args) = VADTConstruct cn (substValue n x t) $ substValue n x <$> args

check :: forall (m :: Type -> Type). MonadThrow m => [Statement Term] -> m ()
check = void . foldlM process []
  where process :: [(Name, Value)] -> Statement Term -> m [(Name, Value)]
        process env (Define n t x) = checkTerm env x t' $> (Name n, t'):env
          where t' = evalTerm t
        process env (Debug _) = pure env
        process env (Check _) = pure env
        process _ _ = error "unimplemented"

interpret :: forall (m :: Type -> Type). (MonadThrow m, MonadIO m) => [(Name, Value, Value)] -> [Statement Term] -> m [(Name, Value, Value)]
interpret e p = check p *> foldlM process e p
  where process :: [(Name, Value, Value)] -> Statement Term -> m [(Name, Value, Value)]
        process env (Define n t x) = pure $ (Name n, t', x'):env
          where x' = substAll (terms env) $ evalTerm x
                t' = substAll (terms env) $ evalTerm t
        process env (Debug x) = (liftIO . putStrLn . pretty $ quote x') $> env
          where x' = substAll (terms env) $ evalTerm x
        process env (Check x) = do
          t <- typeOfTerm (types env) x
          (liftIO . putStrLn . pretty $ quote t) $> env
        process _ _ = error "unimplemented"
        substAll :: [(Name, Value)] -> Value -> Value
        substAll [] x = x
        substAll ((n, v):env) x = substAll env $ substValue n v x
        terms :: [(Name, Value, Value)] -> [(Name, Value)]
        terms = fmap (\(n, _, x) -> (n, x))
        types :: [(Name, Value, Value)] -> [(Name, Value)]
        types = fmap (\(n, t, _) -> (n, t))
