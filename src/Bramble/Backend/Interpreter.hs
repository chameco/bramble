module Bramble.Backend.Interpreter where

import GHC.Num ((-))

import Control.Applicative (pure, (*>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Safe (MonadThrow)

import Data.Kind (Type)
import Data.Monoid ((<>))
import Data.Functor (fmap, void, (<$>), ($>))
import Data.Foldable (foldlM)
import Data.Function (($), (.))
import Data.List (length)
import Data.Int (Int)
import Data.Text (Text)
import Data.Text.IO (putStrLn)

import Bramble.Utility.Pretty
import Bramble.Core.Calculus
import Bramble.Core.Inductive
import Bramble.Core.Vernacular

substAll :: [(Name, Value)] -> Value -> Value
substAll [] x = x
substAll ((n, v):env) x = substAll env $ substValue n v x

curryConstructor :: Int -> Text -> Value -> [Value] -> Value
curryConstructor 0 cn t args = VADTConstruct cn t args
curryConstructor i cn t args = VLambda $ \x -> curryConstructor (i - 1) cn t (x:args)

curryConstructorType :: [Value] -> Value -> Value
curryConstructorType [] t = t
curryConstructorType (t:ts) t' = VPi t $ \_ -> curryConstructorType ts t'

buildADT :: [(Name, Value, Value)] -> Text -> Sum Value -> [(Name, Value, Value)]
buildADT env n s = buildSum (substAll (terms env') <$> s) <> env'
  where s' = substAll (terms $ (Name n, VStar, VNeutral $ NFree Self):env) <$> s
        t = VADT n s'
        env' = (Name n, VStar, t):env
        t' = substAll (terms env') t
        buildSum :: Sum Value -> [(Name, Value, Value)]
        buildSum (Sum ps) = buildProduct <$> ps
        buildProduct :: Product Value -> (Name, Value, Value)
        buildProduct (Product cn ts) = ( Name cn
                                       , curryConstructorType ts t'
                                       , curryConstructor (length ts) cn t' []
                                       )

terms :: [(Name, Value, Value)] -> [(Name, Value)]
terms = fmap (\(n, _, x) -> (n, x))
types :: [(Name, Value, Value)] -> [(Name, Value)]
types = fmap (\(n, t, _) -> (n, t))

check :: forall (m :: Type -> Type). MonadThrow m => [Statement Term] -> m ()
check = void . foldlM process []
  where process :: [(Name, Value, Value)] -> Statement Term -> m [(Name, Value, Value)]
        process env (Define n t x) = checkTerm (types env) x t' $> (Name n, t', x'):env
          where x' = substAll (terms env) $ evalTerm x
                t' = substAll (terms env) $ evalTerm t
        process env (Debug _) = pure env
        process env (Check _) = pure env
        process env (Data n s) = pure $ buildADT env n $ evalTerm <$> s

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
        process env (Data n s) = pure $ buildADT env n $ evalTerm <$> s
