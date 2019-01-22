module Bramble.Backend.Interpreter where

import GHC.Num ((-))

import Control.Applicative (pure, (*>))
import Control.Monad (mapM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Arrow (second)
import Control.Exception.Safe (MonadThrow)

import Data.Kind (Type)
import Data.Monoid (mconcat, (<>))
import Data.Functor (fmap, void, (<$>), ($>))
import Data.Foldable (foldlM)
import Data.Function (($), (.))
import Data.Tuple (fst)
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

curryADTWrap :: [(Text, Value)] -> Value -> Value
curryADTWrap [] v = v
curryADTWrap ((n, _):ps) v = VLambda $ \x -> substValue (Name n) x $ curryADTWrap ps v

curryADTTypeWrap :: [(Text, Value)] -> Value -> Value
curryADTTypeWrap [] t = t
curryADTTypeWrap ((n, t):ps) t' = VPi t $ \x -> substValue (Name n) x $ curryADTTypeWrap ps t'

curryConstructor :: Int -> Text -> Value -> [Value] -> Value
curryConstructor 0 cn t args = VADTConstruct cn t args
curryConstructor i cn t args = VLambda $ \x -> curryConstructor (i - 1) cn t (x:args)

curryConstructorType :: [Value] -> Value -> Value
curryConstructorType [] t = t
curryConstructorType (t:ts) t' = VPi t $ \_ -> curryConstructorType ts t'

buildADT :: [(Name, Value, Value)] -> Text -> [(Text, Value)] -> Sum Value -> [(Name, Value, Value)]
buildADT env n ps s = buildSum s' <> ((Name n, wrapType VStar, wrapMu $ wrapTerm t'):env)
  where t = VADT n (VNeutral . NFree . Name . fst <$> ps) s
        t' = substAll (terms env) t
        s' = substAll (terms env) <$> s
        wrapMu v = VMu $ \fix -> substValue (Name n) fix v
        wrapConstructor = substValue (Name n) (wrapMu $ wrapTerm t')
        wrapTerm = curryADTWrap ps
        wrapType = curryADTTypeWrap ps
        buildSum :: Sum Value -> [(Name, Value, Value)]
        buildSum (Sum prods) = buildProduct <$> prods
        buildProduct :: Product Value -> (Name, Value, Value)
        buildProduct (Product cn ts) = ( Name cn
                                       , wrapConstructor . wrapType . curryConstructorType ts $ wrapMu t'
                                       , wrapTerm $ curryConstructor (length ts) cn (wrapMu t') []
                                       )

terms :: [(Name, Value, Value)] -> [(Name, Value)]
terms = fmap (\(n, _, x) -> (n, x))
types :: [(Name, Value, Value)] -> [(Name, Value)]
types = fmap (\(n, t, _) -> (n, t))

check :: forall (m :: Type -> Type). (MonadThrow m, MonadIO m) => [(Name, Value, Value)] -> [Statement Term] -> m ()
check e = void . foldlM process e
  where process :: [(Name, Value, Value)] -> Statement Term -> m [(Name, Value, Value)]
        process env (Define n t x) = checkTerm ((Name n, t'):types env) x t' $> (Name n, t', x'):env
          where x' = VNeutral . NFix t' . VLambda $ \fix -> substValue (Name n) fix . substAll (terms env) $ evalTerm x
                t' = substAll (terms env) $ evalTerm t
        process env (Debug _) = pure env
        process env (Infer _) = pure env
        process env (Check _ _) = pure env
        process env (Data n ps s) = pure $ buildADT env n (second evalTerm <$> ps) $ evalTerm <$> s
        process env Env = pure env

interpret :: forall (m :: Type -> Type). (MonadThrow m, MonadIO m) => [(Name, Value, Value)] -> [Statement Term] -> m [(Name, Value, Value)]
interpret e p = check e p *> foldlM process e p
  where process :: [(Name, Value, Value)] -> Statement Term -> m [(Name, Value, Value)]
        process env (Define n t x) = do
          liftIO . putStrLn . pretty $ quote x'
          pure $ (Name n, t', x'):env
          where x' = VNeutral . NFix t' . VLambda $ \fix -> substValue (Name n) fix . substAll (terms env) $ evalTerm x
                t' = substAll (terms env) $ evalTerm t
        process env (Data n ps s) = pure $ buildADT env n (second evalTerm <$> ps) $ evalTerm <$> s
        process env (Debug x) = (liftIO . putStrLn . pretty $ quote x') $> env
          where x' = substAll (terms env) $ evalTerm x
        process env (Infer x) = do
          t' <- typeOfTerm (types env) . TermCheck $ quote x'
          liftIO . putStrLn . pretty $ quote t'
          pure env
          where x' = substAll (terms env) $ evalTerm x
        process env (Check x t) = do
          checkTerm (types env) (TermCheck $ quote t') VStar
          checkTerm (types env) (TermCheck $ quote x') t'
          pure env
          where x' = substAll (terms env) $ evalTerm x
                t' = substAll (terms env) $ evalTerm t
        process env Env = liftIO (mapM_ (putStrLn . printEnv) env) $> env
          where printEnv :: (Name, Value, Value) -> Text
                printEnv (n, t, x) = mconcat [pretty n, " := ", pretty $ quote x, " : ", pretty $ quote t]
