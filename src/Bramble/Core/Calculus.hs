module Bramble.Core.Calculus where

import GHC.Num ((+), (-))
import GHC.Err (error)

import Control.Applicative (pure)
import Control.Monad (mapM_, forM_, unless)
import Control.Arrow (second)
import Control.Exception.Safe (MonadThrow, throw)

import Data.Monoid (mconcat, (<>))
import Data.Functor (fmap, (<$>))
import Data.Foldable (foldr)
import Data.Eq (Eq, (==))
import Data.Function (flip, const, ($), (.))
import Data.Maybe (Maybe(..))
import Data.List (null, length, zip, lookup, (!!))
import Data.Bool (otherwise)
import Data.Int (Int)
import Data.Text (Text, pack, unwords, intercalate)

import Text.Show (Show, show)

import Bramble.Utility.Pretty
import Bramble.Utility.Error
import Bramble.Core.Inductive

validateSum :: forall m. MonadThrow m => Int -> [(Name, Value)] -> Sum TermCheck -> m ()
validateSum i env (Sum ps) = mapM_ vp ps
  where vp :: Product TermCheck -> m ()
        vp (Product _ ts) = mapM_ (\x -> typeCheck i env x VStar) ts

data Name where
  Name :: Text -> Name
  Self :: Name
  Quote :: Int -> Name
  Local :: Int -> Name
deriving instance Show Name
deriving instance Eq Name

instance Pretty Name where
  pretty (Name n) = n
  pretty Self = "<self>"
  pretty (Quote i) = mconcat ["<quote-", pack $ show i, ">"]
  pretty (Local i) = mconcat ["<local-", pack $ show i, ">"]

data TermInf where
  Annotate :: TermCheck -> TermCheck -> TermInf
  Star :: TermInf
  Pi :: TermCheck -> TermCheck -> TermInf
  Bound :: Int -> TermInf
  Free :: Name -> TermInf
  Apply :: TermInf -> TermCheck -> TermInf

  ADT :: Text -> [TermCheck] -> Sum TermCheck -> TermInf
  ADTConstruct :: Text -> TermCheck -> [TermCheck] -> TermInf
  ADTEliminate :: TermInf -> [(Text, TermCheck)] -> TermInf
deriving instance Show TermInf
deriving instance Eq TermInf

instance Pretty TermInf where
  pretty (Annotate e t) = mconcat ["[", pretty e, " : ", pretty t, "]"]
  pretty Star = "Type"
  pretty (Pi t b) = mconcat ["Π", pretty t, ".", pretty b]
  pretty (Bound i) = pack $ show i
  pretty (Free n) = pretty n
  pretty (Apply f x) = mconcat ["(", pretty f, " ", pretty x, ")"]
  pretty (ADT n ps s) = mconcat ["{", n, "(", intercalate ", " $ pretty <$> ps, ")", " = ", pretty s, "}"]
  pretty (ADTConstruct cn _ args) = "{" <> cn <> (if null args then "" else " ") <> unwords (pretty <$> args) <> "}"
  pretty (ADTEliminate x hs) = "{" <> pretty x <> " ! " <> intercalate "; " ((\(cn, b) -> cn <> " -> " <> pretty b) <$> hs) <> "}"

data TermCheck where
  Inf :: TermInf -> TermCheck
  Lambda :: TermCheck -> TermCheck
deriving instance Show TermCheck
deriving instance Eq TermCheck

instance Pretty TermCheck where
  pretty (Inf x) = pretty x
  pretty (Lambda b) = "λ." <> pretty b

data Neutral where
  NFree :: Name -> Neutral
  NApply :: Neutral -> Value -> Neutral
  NADTEliminate :: Neutral -> [(Text, Value)] -> Neutral

data Value where
  VLambda :: (Value -> Value) -> Value
  VStar :: Value
  VPi :: Value -> (Value -> Value) -> Value
  VNeutral :: Neutral -> Value

  VADT :: Text -> [Value] -> Sum Value -> Value
  VADTConstruct :: Text -> Value -> [Value] -> Value

vApply :: Value -> Value -> Value
vApply (VLambda b) v = b v
vApply (VNeutral n) v = VNeutral $ NApply n v
vApply _ _ = error "malformed expression"

vADTEliminate :: Value -> [(Text, Value)] -> Value
vADTEliminate (VADTConstruct cn (VADT _ _ (Sum ps)) args) hs =
  if length ps == length hs
  then case lookup cn hs of
    Just body -> foldr (flip vApply) body args
    _ -> error "malformed expression"
  else error "malformed expression"
vADTEliminate (VNeutral n) hs = VNeutral $ NADTEliminate n hs
vADTEliminate _ _ = error "malformed expression"

substNeutral :: Name -> Value -> Neutral -> Value
substNeutral n x (NFree n')
  | n == n' = x
  | otherwise = VNeutral $ NFree n'
substNeutral n x (NApply m y) = vApply (substNeutral n x m) $ substValue n x y
substNeutral n x (NADTEliminate m hs) = vADTEliminate (substNeutral n x m) $ second (substValue n x) <$> hs

substValue :: Name -> Value -> Value -> Value
substValue n x (VLambda f) = VLambda $ substValue n x . f
substValue _ _ VStar = VStar
substValue n x (VPi t f) = VPi (substValue n x t) $ substValue n x . f
substValue n x (VNeutral m) = substNeutral n x m
substValue n x (VADT n' ps s) = VADT n' (substValue n x <$> ps) $ substValue n x <$> s
substValue n x (VADTConstruct cn t args) = VADTConstruct cn (substValue n x t) $ substValue n x <$> args

evalInf :: TermInf -> [Value] -> Value
evalInf (Annotate e _) env = evalCheck e env
evalInf Star _ = VStar
evalInf (Pi t b) env = VPi (evalCheck t env) $ \x -> evalCheck b (x:env)
evalInf (Free n) _ = VNeutral (NFree n)
evalInf (Bound i) env = env !! i
evalInf (Apply f x) env = vApply (evalInf f env) $ evalCheck x env
evalInf (ADT n ps s) env = VADT n ((`evalCheck` env) <$> ps) (flip evalCheck env <$> s)
evalInf (ADTConstruct cn t args) env = VADTConstruct cn (evalCheck t env) $ flip evalCheck env <$> args
evalInf (ADTEliminate x hs) env = vADTEliminate (evalInf x env) $ second (`evalCheck` env) <$> hs

evalCheck :: TermCheck -> [Value] -> Value
evalCheck (Inf x) env = evalInf x env
evalCheck (Lambda b) env = VLambda $ \x -> evalCheck b (x:env)

substInf :: Int -> TermInf -> TermInf -> TermInf
substInf n x (Annotate e t) = Annotate (substCheck n x e) (substCheck n x t)
substInf n x (Pi t b) = Pi (substCheck n x t) $ substCheck (n + 1) x b
substInf n x (Bound i) | i == n = x
                       | otherwise = Bound i
substInf n x (Apply f y) = Apply (substInf n x f) $ substCheck n x y
substInf n x (ADTConstruct cn t args) = ADTConstruct cn t $ substCheck n x <$> args
substInf n x (ADTEliminate y hs) = ADTEliminate (substInf n x y) $ fmap (second $ substCheck n x) hs
substInf _ _ b = b

substCheck :: Int -> TermInf -> TermCheck -> TermCheck
substCheck n x (Inf y) = Inf $ substInf n x y
substCheck n x (Lambda b) = Lambda $ substCheck (n + 1) x b

quote :: Value -> TermCheck
quote = go 0
  where go :: Int -> Value -> TermCheck
        go i (VLambda f) = Lambda . go (i + 1) . f . VNeutral . NFree $ Quote i
        go _ VStar = Inf Star
        go i (VPi v f) = Inf . Pi (go i v) . go (i + 1) . f . VNeutral . NFree $ Quote i
        go i (VNeutral n) = Inf $ nq i n
        go i (VADT n ps s) = Inf $ ADT n (go i <$> ps) $ go i <$> s
        go i (VADTConstruct n t args) = Inf . ADTConstruct n (go i t) $ go i <$> args
        nq :: Int -> Neutral -> TermInf
        nq i (NFree x) = boundfree i x
        nq i (NApply n v) = Apply (nq i n) $ go i v
        nq i (NADTEliminate n hs) = ADTEliminate (nq i n) $ second (go i) <$> hs
        boundfree :: Int -> Name -> TermInf
        boundfree i (Quote k) = Bound $ i - k - 1
        boundfree _ x = Free x

typeInf :: MonadThrow m  => Int -> [(Name, Value)] -> TermInf -> m Value
typeInf i env (Annotate e t) = do
  typeCheck i env t VStar
  let t' = evalCheck t []
  typeCheck i env e t'
  pure t'
typeInf _ _ Star = pure VStar
typeInf i env (Pi t b) = do
  typeCheck i env t VStar
  let t' = evalCheck t []
  typeCheck (i + 1) ((Local i, t'):env) (substCheck 0 (Free $ Local i) b) VStar
  pure VStar
typeInf _ _ (Bound i) = throw $ UnboundIndex i
typeInf _ env (Free n) = case lookup n env of
  Just t -> pure t
  Nothing -> throw . UnknownIdentifier $ pretty n
typeInf i env (Apply f x) = do
  ft <- typeInf i env f
  case ft of
    VPi t b -> do
      typeCheck i env x t
      pure . b $ evalCheck x []
    _ -> throw . IllegalApplication (pretty f) (pretty $ quote ft) $ pretty x
typeInf i env (ADT _ _ s) = do
  validateSum i ((Self, VStar):env) s
  pure VStar
typeInf i env (ADTConstruct cn t args) = case evalCheck t [] of
  t'@(VADT n _ s) ->
    case lookupProduct cn s of
      Just (Product _ fs) ->
        if length args == length fs
        then do forM_ (zip fs args) $ \(ft, a) -> typeCheck i env a ft
                pure t'
        else throw . ConstructorArityMismatch cn (length fs) $ length args
      Nothing -> throw $ InvalidConstructor n cn
  _ -> throw $ ConstructNonADT (pretty t) cn
typeInf i env (ADTEliminate x hs) = do
  xt <- typeInf i env x
  case xt of
    VADT n _ s@(Sum prods) -> case hs of
      ((_, h1):_) -> 
        if length prods == length hs
        then do
          t <- typeInf i env $ codomain h1
          forM_ hs $ \(cn, h) ->
            case lookupProduct cn $ substValue Self xt <$> s of
              Just (Product _ p) ->
                typeCheck i env h $ foldr (\pt b -> VPi pt $ const b) t p
              _ -> throw $ InvalidConstructor n cn
          pure t
        else throw . MissingCases n (length prods) $ length hs
      [] -> throw EmptyCase
    _ -> throw . EliminateNonADT (pretty x) . pretty $ quote xt
  where codomain :: TermCheck -> TermInf
        codomain (Lambda b) = codomain b
        codomain (Inf y) = y
typeCheck :: MonadThrow m => Int -> [(Name, Value)] -> TermCheck -> Value -> m ()
typeCheck i env (Inf x) v = do
  v' <- typeInf i env x
  unless (quote v == quote v') . throw . TypeMismatch (pretty $ quote v) (pretty x) . pretty $ quote v'
typeCheck i env (Lambda b) (VPi t b') =
  typeCheck (i + 1) ((Local i, t):env) (substCheck 0 (Free (Local i)) b) . b' . VNeutral . NFree $ Local i
typeCheck _ _ x v = throw . StructuralTypeMismatch (pretty $ quote v) $ pretty x

data Term = TermInf TermInf
          | TermCheck TermCheck
          deriving (Show, Eq)

instance Pretty Term where
  pretty = pretty . repr

repr :: Term -> TermCheck
repr (TermInf x) = Inf x
repr (TermCheck x) = x

evalTerm :: Term -> Value
evalTerm (TermInf x) = evalInf x []
evalTerm (TermCheck x) = evalCheck x []

nameOfTerm :: Term -> Maybe Name
nameOfTerm (TermInf (Free n)) = Just n
nameOfTerm (TermCheck (Inf (Free n))) = Just n
nameOfTerm _ = Nothing

typeOfTerm :: MonadThrow m => [(Name, Value)] -> Term -> m Value
typeOfTerm env (TermInf x) = typeInf 0 env x
typeOfTerm env (TermCheck (Inf x)) = typeInf 0 env x
typeOfTerm _ (TermCheck x) = throw . CannotInferType $ pretty x

checkTerm :: MonadThrow m => [(Name, Value)] -> Term -> Value -> m ()
checkTerm env x = typeCheck 0 env $ repr x
