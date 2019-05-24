module Bramble.Core.Calculus where

import GHC.Num ((+), (-))
import GHC.Err (error)

import Control.Applicative (pure)
import Control.Monad (mapM_, forM_, when, unless)
import Control.Arrow (second)
import Control.Exception.Safe (MonadThrow, throw)

import Data.Monoid (mconcat, (<>))
import Data.Functor (Functor, (<$>), ($>))
import Data.Foldable (Foldable, foldr, elem)
import Data.Traversable (Traversable)
import Data.Eq (Eq, (==))
import Data.Function (flip, const, ($), (.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (null, length, zip, lookup, (!!))
import Data.Bool (Bool(..), otherwise)
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
  Quote :: Int -> Name
  Local :: Int -> Name
deriving instance Show Name
deriving instance Eq Name

instance Pretty Name where
  pretty (Name n) = n
  pretty (Quote i) = mconcat ["<quote-", pack $ show i, ">"]
  pretty (Local i) = mconcat ["<local-", pack $ show i, ">"]

data Native ty where
  NativeType :: Text -> Native ty
  NativeInt :: ty -> Int -> Native ty
  NativeString :: ty -> Text -> Native ty
  NativeFunction :: ty -> ty -> Text -> Native ty
deriving instance Show ty => Show (Native ty)
deriving instance Eq ty => Eq (Native ty)
deriving instance Functor Native
deriving instance Foldable Native
deriving instance Traversable Native

data TermInf where
  Annotate :: TermCheck -> TermCheck -> TermInf
  Star :: TermInf
  Pi :: TermCheck -> TermCheck -> TermInf
  Bound :: Int -> TermInf
  Free :: Name -> TermInf
  Apply :: TermInf -> TermCheck -> TermInf
  Fix :: TermCheck -> TermCheck -> TermInf

  ADT :: Text -> [TermCheck] -> Sum TermCheck -> TermInf
  ADTConstruct :: Text -> TermCheck -> [TermCheck] -> TermInf
  ADTEliminate :: TermInf -> [(Text, TermCheck)] -> TermInf

  Row :: Bool -> [(Text, TermCheck)] -> [Text] -> TermInf
  RowEliminate :: TermInf -> Text -> TermInf

  Foreign :: Native TermCheck -> TermInf
deriving instance Show TermInf
deriving instance Eq TermInf

instance Pretty TermInf where
  pretty (Annotate e t) = mconcat ["[", pretty e, " : ", pretty t, "]"]
  pretty Star = "Type"
  pretty (Pi t b) = mconcat ["Π", pretty t, ".", pretty b]
  pretty (Bound i) = pack $ show i
  pretty (Free n) = pretty n
  pretty (Apply f x) = mconcat ["(", pretty f, " ", pretty x, ")"]
  pretty (Fix t f) = mconcat ["Y(", pretty t, ", ", pretty f, ")"]
  pretty (ADT n ps s) = mconcat ["{", n, "(", intercalate ", " $ pretty <$> ps, ")", " = ", pretty s, "}"]
  pretty (ADTConstruct cn _ args) = "{" <> cn <> (if null args then "" else " ") <> unwords (pretty <$> args) <> "}"
  pretty (ADTEliminate x hs) = "{" <> pretty x <> " ! " <> intercalate "; " ((\(cn, b) -> cn <> " -> " <> pretty b) <$> hs) <> "}"
  pretty (Row e fs es) = mconcat [ "{"
                                 , intercalate ", " ((\(fn, ft) -> fn <> " : " <> pretty ft) <$> fs)
                                 , if null es then "" else ", "
                                 , intercalate ", " (("exclude "<>) <$> es)
                                 , if e then " ... }" else "}"
                                 ]
  pretty (RowEliminate x fn) = pretty x <> "." <> fn
  pretty (Foreign n) = mconcat ["<foreign: ", pack $ show n, ">"]

data TermCheck where
  Inf :: TermInf -> TermCheck
  Lambda :: TermCheck -> TermCheck
  RowConstruct :: [(Text, TermCheck)] -> TermCheck
deriving instance Show TermCheck
deriving instance Eq TermCheck

instance Pretty TermCheck where
  pretty (Inf x) = pretty x
  pretty (Lambda b) = "λ." <> pretty b
  pretty (RowConstruct fs) = "{" <> intercalate "; " ((\(fn, x) -> fn <> " := " <> pretty x) <$> fs) <> "}"

data Neutral where
  NFree :: Name -> Neutral
  NApply :: Neutral -> Value -> Neutral
  NFix :: Value -> Value -> Neutral
  NADTEliminate :: Neutral -> [(Text, Value)] -> Neutral
  NRowEliminate :: Neutral -> Text -> Neutral
  NForeign :: Native Value -> Neutral

data Value where
  VLambda :: (Value -> Value) -> Value
  VStar :: Value
  VPi :: Value -> (Value -> Value) -> Value
  VNeutral :: Neutral -> Value

  VADT :: Text -> [Value] -> Sum Value -> Value
  VADTConstruct :: Text -> Value -> [Value] -> Value

  VRow :: Bool -> [(Text, Value)] -> [Text] -> Value
  VRowConstruct :: [(Text, Value)] -> Value

unfixing :: Value -> Value
unfixing (VNeutral (NFix t v@(VLambda b))) = b . VNeutral $ NFix t v
unfixing v = v

nApply :: Neutral -> Value -> Value
nApply (NFix t v@(VLambda b)) x = vApply (b . VNeutral $ NFix t v) x
nApply n x = VNeutral $ NApply n x

vApply :: Value -> Value -> Value
vApply (VLambda b) v = b v
vApply (VNeutral n) v = nApply n v
vApply _ _ = error "malformed expression"

nADTEliminate :: Neutral -> [(Text, Value)] -> Value
nADTEliminate (NFix t v@(VLambda b)) hs = vADTEliminate (b . VNeutral $ NFix t v) hs
nADTEliminate n hs = VNeutral $ NADTEliminate n hs

vADTEliminate :: Value -> [(Text, Value)] -> Value
vADTEliminate (VADTConstruct cn t args) hs =
  case unfixing t of
    VADT _ _ (Sum ps) ->
      if length ps == length hs
      then case lookup cn hs of
        Just body -> foldr (flip vApply) body args
        _ -> error "malformed expression"
      else error "malformed expression"
    _ -> error "malformed expression"
vADTEliminate (VNeutral n) hs = nADTEliminate n hs
vADTEliminate _ _ = error "malformed expression"

nRowEliminate :: Neutral -> Text -> Value
nRowEliminate (NFix t v@(VLambda b)) fn = vRowEliminate (b . VNeutral $ NFix t v) fn
nRowEliminate n fn = VNeutral $ NRowEliminate n fn

vRowEliminate :: Value -> Text -> Value
vRowEliminate (VRowConstruct fs) fn = fromMaybe (error "malformed expression") $ lookup fn fs
vRowEliminate (VNeutral n) fn = VNeutral $ NRowEliminate n fn
vRowEliminate _ _ = error "malformed expression"
    
substNeutral :: Name -> Value -> Neutral -> Value
substNeutral n x (NFree n')
  | n == n' = x
  | otherwise = VNeutral $ NFree n'
substNeutral n x (NApply m y) = vApply (substNeutral n x m) $ substValue n x y
substNeutral n x (NFix t v) = VNeutral . NFix (substValue n x t) $ substValue n x v
substNeutral n x (NADTEliminate m hs) = vADTEliminate (substNeutral n x m) $ second (substValue n x) <$> hs
substNeutral n x (NRowEliminate m fn) = vRowEliminate (substNeutral n x m) fn
substNeutral n x (NForeign (NativeFunction i o name)) = VNeutral . NForeign $ NativeFunction (substValue n x i) (substValue n x o) name
substNeutral n x (NForeign n') = VNeutral . NForeign $ substValue n x <$> n'

substValue :: Name -> Value -> Value -> Value
substValue n x (VLambda f) = VLambda $ substValue n x . f
substValue _ _ VStar = VStar
substValue n x (VPi t f) = VPi (substValue n x t) $ substValue n x . f
substValue n x (VNeutral m) = substNeutral n x m
substValue n x (VADT n' ps s) = VADT n' (substValue n x <$> ps) $ substValue n x <$> s
substValue n x (VADTConstruct cn t args) = VADTConstruct cn (substValue n x t) $ substValue n x <$> args
substValue n x (VRow e fs es) = VRow e (second (substValue n x) <$> fs) es
substValue n x (VRowConstruct fs) = VRowConstruct $ second (substValue n x) <$> fs

evalInf :: TermInf -> [Value] -> Value
evalInf (Annotate e _) env = evalCheck e env
evalInf Star _ = VStar
evalInf (Pi t b) env = VPi (evalCheck t env) $ \x -> evalCheck b (x:env)
evalInf (Free n) _ = VNeutral $ NFree n
evalInf (Bound i) env = env !! i
evalInf (Apply f x) env = vApply (evalInf f env) $ evalCheck x env
evalInf (Fix t f) env = VNeutral . NFix (evalCheck t env) $ evalCheck f env
evalInf (ADT n ps s) env = VADT n ((`evalCheck` env) <$> ps) (flip evalCheck env <$> s)
evalInf (ADTConstruct cn t args) env = VADTConstruct cn (evalCheck t env) $ flip evalCheck env <$> args
evalInf (ADTEliminate x hs) env = vADTEliminate (evalInf x env) $ second (`evalCheck` env) <$> hs
evalInf (Row e fs es) env = VRow e (second (`evalCheck` env) <$> fs) es
evalInf (RowEliminate x fn) env = vRowEliminate (evalInf x env) fn
evalInf (Foreign n) env = VNeutral . NForeign $ (`evalCheck` env) <$> n

evalCheck :: TermCheck -> [Value] -> Value
evalCheck (Inf x) env = evalInf x env
evalCheck (Lambda b) env = VLambda $ \x -> evalCheck b (x:env)
evalCheck (RowConstruct fs) env = VRowConstruct $ second (`evalCheck` env) <$> fs

substInf :: Int -> TermInf -> TermInf -> TermInf
substInf n x (Annotate e t) = Annotate (substCheck n x e) (substCheck n x t)
substInf _ _ Star = Star
substInf n x (Pi t b) = Pi (substCheck n x t) $ substCheck (n + 1) x b
substInf _ _ v@Free{} = v
substInf n x (Bound i) | i == n = x
                       | otherwise = Bound i
substInf n x (Apply f y) = Apply (substInf n x f) $ substCheck n x y
substInf n x (Fix t f) = Fix (substCheck n x t) $ substCheck n x f
substInf n x (ADT n' ps s) = ADT n' (substCheck n x <$> ps) $ substCheck n x <$> s
substInf n x (ADTConstruct cn t args) = ADTConstruct cn t $ substCheck n x <$> args
substInf n x (ADTEliminate y hs) = ADTEliminate (substInf n x y) $ second (substCheck n x) <$> hs
substInf n x (Row e fs es) = Row e (second (substCheck n x) <$> fs) es
substInf n x (RowEliminate y fn) = RowEliminate (substInf n x y) fn
substInf n x (Foreign n') = Foreign $ substCheck n x <$> n'

substCheck :: Int -> TermInf -> TermCheck -> TermCheck
substCheck n x (Inf y) = Inf $ substInf n x y
substCheck n x (Lambda b) = Lambda $ substCheck (n + 1) x b
substCheck n x (RowConstruct fs) = RowConstruct $ second (substCheck n x) <$> fs

quote :: Value -> TermCheck
quote = go 0
  where go :: Int -> Value -> TermCheck
        go i (VLambda f) = Lambda . go (i + 1) . f . VNeutral . NFree $ Quote i
        go _ VStar = Inf Star
        go i (VPi v f) = Inf . Pi (go i v) . go (i + 1) . f . VNeutral . NFree $ Quote i
        go i (VNeutral n) = Inf $ nq i n
        go i (VADT n ps s) = Inf $ ADT n (go i <$> ps) $ go i <$> s
        go i (VADTConstruct n t args) = Inf . ADTConstruct n (go i t) $ go i <$> args
        go i (VRow e fs es) = Inf $ Row e (second (go i) <$> fs) es
        go i (VRowConstruct fs) = RowConstruct $ second (go i) <$> fs
        nq :: Int -> Neutral -> TermInf
        nq i (NFree x) = boundfree i x
        nq i (NApply n v) = Apply (nq i n) $ go i v
        nq i (NFix t v) = Fix (go i t) $ go i v
        nq i (NADTEliminate n hs) = ADTEliminate (nq i n) $ second (go i) <$> hs
        nq i (NRowEliminate n fn) = RowEliminate (nq i n) fn
        nq i (NForeign n) = Foreign $ go i <$> n
        boundfree :: Int -> Name -> TermInf
        boundfree i (Quote k) = Bound $ i - k - 1
        boundfree _ x = Free x

typeInf :: MonadThrow m => Int -> [(Name, Value)] -> TermInf -> m Value
typeInf i env (Annotate e t) = do
  typeCheck i env t VStar
  let t' = unfixing $ evalCheck t []
  typeCheck i env e t'
  pure t'
typeInf _ _ Star = pure VStar
typeInf i env (Pi t b) = do
  typeCheck i env t VStar
  let t' = unfixing $ evalCheck t []
  typeCheck (i + 1) ((Local i, t'):env) (substCheck 0 (Free $ Local i) b) VStar
  pure VStar
typeInf _ _ (Bound i) = throw $ UnboundIndex i
typeInf _ env (Free n) = case lookup n env of
  Just t -> pure t
  Nothing -> throw . UnknownIdentifier $ pretty n
typeInf i env (Apply f x) = do
  ft <- typeInf i env f
  case unfixing ft of
    VPi t b -> do
      typeCheck i env x $ unfixing t
      pure . b $ evalCheck x []
    _ -> throw . IllegalApplication (pretty f) (pretty $ quote ft) $ pretty x
typeInf i env (Fix t f) = do
  typeCheck i env t VStar
  let t' = unfixing $ evalCheck t []
  typeCheck i env f . VPi t' $ const t'
  pure t'
typeInf i env (ADT _ _ s) = validateSum i env s $> VStar
typeInf i env (ADTConstruct cn t args) = case unfixing $ evalCheck t [] of
  t'@(VADT n _ s) ->
    case lookupProduct cn s of
      Just (Product _ fs) ->
        if length args == length fs
        then do forM_ (zip fs args) $ \(ft, a) -> typeCheck i env a $ unfixing ft
                pure t'
        else throw . ConstructorArityMismatch cn (length fs) $ length args
      Nothing -> throw $ InvalidConstructor n cn
  _ -> throw $ ConstructNonADT (pretty t) cn
typeInf i env elim@(ADTEliminate x hs) = do
  xt <- typeInf i env x
  case unfixing xt of
    VADT n _ s@(Sum prods) -> case hs of
      ((_, h1):_) -> 
        if length prods == length hs
        then case typeInf i env <$> codomain h1 of
               Just mt -> do
                 t <- mt
                 forM_ hs $ \(cn, h) ->
                   case lookupProduct cn s of
                     Just (Product _ p) ->
                       typeCheck i env h $ foldr (\pt b -> VPi (unfixing pt) $ const b) (unfixing t) p
                     _ -> throw $ InvalidConstructor n cn
                 pure t
               Nothing -> throw . BadCase (pretty x) $ pretty elim
        else throw . MissingCases n (length prods) $ length hs
      [] -> throw EmptyCase
    _ -> throw . EliminateNonADT (pretty x) . pretty $ quote xt
  where codomain :: TermCheck -> Maybe TermInf
        codomain (Lambda b) = codomain b
        codomain (Inf y) = Just y
        codomain _ = Nothing
typeInf i env r@(Row _ fs es) = do
  forM_ fs $ \(fn, ft) ->
    if fn `elem` es
    then throw . ImpossibleRow fn $ pretty r
    else typeCheck i env ft VStar
  pure VStar
typeInf i env (RowEliminate x fn) = do
  xt <- typeInf i env x
  case unfixing xt of
    VRow _ fs _ ->
      case lookup fn fs of
        Just t -> pure t
        _ -> throw . MissingField fn (pretty x) . pretty $ quote xt
    _ -> throw . EliminateNonRow fn (pretty x) . pretty $ quote xt
typeInf i env (Foreign n) = case n of
  NativeType _ -> pure VStar
  NativeInt t _ -> typeCheck i env t VStar $> evalCheck t []
  NativeString t _ -> typeCheck i env t VStar $> evalCheck t []
  NativeFunction dom cod _ -> do
    typeCheck i env dom VStar
    typeCheck i env cod VStar
    pure $ VPi (evalCheck dom []) (const $ evalCheck cod [])

typeCheck :: MonadThrow m => Int -> [(Name, Value)] -> TermCheck -> Value -> m ()
typeCheck i env (Inf x) v = do
  v' <- typeInf i env x
  unless (quote (unfixing v) == quote (unfixing v')) . throw . TypeMismatch (pretty $ quote (unfixing v)) (pretty x) . pretty $ quote (unfixing v')
typeCheck i env (Lambda b) (VPi t b') =
  typeCheck (i + 1) ((Local i, t):env) (substCheck 0 (Free (Local i)) b) . unfixing . b' . VNeutral . NFree $ Local i
typeCheck i env rc@(RowConstruct fs) rt@(VRow e fts es) = do
  forM_ fts $ \(fn, ft) ->
    case lookup fn fs of
      Just x -> typeCheck i env x $ unfixing ft
      Nothing -> throw . MissingField fn (pretty rc) . pretty $ quote rt
  forM_ fs $ \(fn, _) -> do
    when (fn `elem` es) . throw . ExcludedField fn (pretty rc) . pretty $ quote rt
    unless e $ case lookup fn fts of
      Just _ -> pure ()
      _ -> throw . ExcludedField fn (pretty rc) . pretty $ quote rt
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

infTerm :: MonadThrow m => [(Name, Value)] -> Term -> m Value
infTerm env (TermInf x) = typeInf 0 env x
infTerm _ (TermCheck x) = throw . CannotInferType $ pretty x
