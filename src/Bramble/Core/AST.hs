module Bramble.Core.AST where

-- TODO for this evening:
-- improve error messages, use to debug ADTs
-- parameteric polymorphism for ADTs
-- parser

import Prelude

import Control.Monad (forM_, unless)
import Control.Arrow (second)

import Data.Text (Text)

data Product = Product Text [TermCheck]
             deriving (Show, Eq)
newtype Sum = Sum [Product]
            deriving (Show, Eq)

lookupProduct :: Text -> Sum -> Maybe Product
lookupProduct n (Sum ps) = go ps
  where go :: [Product] -> Maybe Product
        go [] = Nothing
        go (p@(Product n' _):r) | n == n' = Just p
                                | otherwise = go r

validateSum :: Int -> [(Name, Value)] -> Sum -> Either Text ()
validateSum i env (Sum ps) = mapM_ vp ps
  where vp :: Product -> Either Text ()
        vp (Product _ ts) = mapM_ (\x -> typeCheck i env x VStar) ts

data Name where
  Name :: Text -> Name
  Quote :: Int -> Name
  Local :: Int -> Name
deriving instance Show Name
deriving instance Eq Name

data TermInf where
  Annotate :: TermCheck -> TermCheck -> TermInf
  Star :: TermInf
  Pi :: TermCheck -> TermCheck -> TermInf
  Bound :: Int -> TermInf
  Free :: Name -> TermInf
  Apply :: TermInf -> TermCheck -> TermInf

  Bool :: TermInf
  BoolTrue :: TermInf
  BoolFalse :: TermInf

  ADT :: Text -> Sum -> TermInf
  ADTConstruct :: Text -> TermCheck -> [TermCheck] -> TermInf
  ADTEliminate :: TermInf -> TermCheck -> [(Text, TermCheck)] -> TermInf
deriving instance Show TermInf
deriving instance Eq TermInf

data TermCheck where
  Inf :: TermInf -> TermCheck
  Lambda :: TermCheck -> TermCheck
deriving instance Show TermCheck
deriving instance Eq TermCheck

data Neutral where
  NFree :: Name -> Neutral
  NApply :: Neutral -> Value -> Neutral
  NADTEliminate :: Neutral -> Neutral -> [(Text, Value)] -> Neutral

data Value where
  VLambda :: (Value -> Value) -> Value
  VStar :: Value
  VPi :: Value -> (Value -> Value) -> Value
  VNeutral :: Neutral -> Value

  VBool :: Value
  VBoolTrue :: Value
  VBoolFalse :: Value

  VADT :: Text -> Sum -> Value
  VADTConstruct :: Text -> Value -> [Value] -> Value

vapp :: Value -> Value -> Value
vapp (VLambda b) v = b v
vapp (VNeutral n) v = VNeutral $ NApply n v
vapp _ _ = error "malformed expression"

evalInf :: TermInf -> [Value] -> Value
evalInf (Annotate e _) env = evalCheck e env
evalInf Star _ = VStar
evalInf (Pi t b) env = VPi (evalCheck t env) $ \x -> evalCheck b (x:env)
evalInf (Free n) _ = VNeutral (NFree n)
evalInf (Bound i) env = env !! i
evalInf (Apply f x) env = vapp (evalInf f env) $ evalCheck x env
evalInf Bool _ = VBool
evalInf BoolTrue _ = VBoolTrue
evalInf BoolFalse _ = VBoolFalse
evalInf (ADT n s) _ = VADT n s
evalInf (ADTConstruct cn t args) env = VADTConstruct cn (evalCheck t env) $ flip evalCheck env <$> args
evalInf (ADTEliminate x t hs) env = case (x', t') of
  (VADTConstruct cn (VADT _ (Sum ps)) args, _) ->
    if length ps == length hs
    then case lookup cn hs of
      Just body -> foldr (flip vapp) (evalCheck body env) args
      _ -> error "malformed expression"
    else error "malformed expression"
  (VNeutral nx, VNeutral nt) -> VNeutral $ NADTEliminate nx nt (second (`evalCheck` env) <$> hs)
  _ -> error "malformed expression"
  where x' = evalInf x env
        t' = evalCheck t env

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
substInf n x (ADTEliminate y t hs) = ADTEliminate (substInf n x y) (substCheck n x t) $ fmap (second $ substCheck n x) hs
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
        go _ VBool = Inf Bool
        go _ VBoolTrue = Inf BoolTrue
        go _ VBoolFalse = Inf BoolFalse
        go _ (VADT n s) = Inf $ ADT n s
        go i (VADTConstruct n t args) = Inf . ADTConstruct n (go i t) $ go i <$> args
        nq :: Int -> Neutral -> TermInf
        nq i (NFree x) = boundfree i x
        nq i (NApply n v) = Apply (nq i n) $ go i v
        nq i (NADTEliminate n t hs) = ADTEliminate (nq i n) (Inf $ nq i t) $ second (go i) <$> hs
        boundfree :: Int -> Name -> TermInf
        boundfree i (Quote k) = Bound $ i - k - 1
        boundfree _ x = Free x

typeInf :: Int -> [(Name, Value)] -> TermInf -> Either Text Value
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
typeInf _ _ Bound{} = Left "unbound variable"
typeInf _ env (Free n) = case lookup n env of
  Just t -> pure t
  Nothing -> Left "unknown identifier"
typeInf i env (Apply f x) = do
  ft <- typeInf i env f
  case ft of
    VPi t b -> do
      typeCheck i env x t
      pure . b $ evalCheck x []
    _ -> Left "illegal application"
typeInf _ _ Bool = pure VStar
typeInf _ _ BoolTrue = pure VBool
typeInf _ _ BoolFalse = pure VBool
typeInf i env (ADT _ s) = do
  validateSum i env s
  pure VStar
typeInf i env (ADTConstruct cn t args) = case evalCheck t [] of
  t'@(VADT _ s) ->
    case lookupProduct cn s of
      Just (Product _ fs) ->
        if length args == length fs
        then do forM_ (zip fs args) $ \(ft, a) -> do
                  let ft' = evalCheck ft []
                  typeCheck i env a ft'
                pure t'
        else Left "constructor arity mismatch"
      Nothing -> Left "ADT does not have constructor"
  _ -> Left "attempt to construct non-ADT type"
typeInf i env (ADTEliminate x t hs) = do
  xt <- typeInf i env x
  let t' = evalCheck t []
  typeCheck i env t VStar
  case xt of
    VADT _ s@(Sum ps) ->
      if length ps == length hs
      then do
        forM_ hs $ \(cn, h) -> do
          typeCheck i env h t'
          case lookupProduct cn s of
            Just (Product _ p) -> typeCheck i env h $ foldr (\pt b -> VPi pt $ const b) t' $ flip evalCheck [] <$> p
            _ -> Left "ADT does not have constructor"
        pure . VPi xt $ const t'
      else Left "missing cases"
    _ -> Left "attempt to eliminate non-ADT type"
typeCheck :: Int -> [(Name, Value)] -> TermCheck -> Value -> Either Text ()
typeCheck i env (Inf x) v = do
  v' <- typeInf i env x
  unless (quote v == quote v') $ Left "type mismatch"
typeCheck i env (Lambda b) (VPi t b') =
  typeCheck (i + 1) ((Local i, t):env) (substCheck 0 (Free (Local i)) b) . b' . VNeutral . NFree $ Local i
typeCheck _ _ _ _ = Left "type mismatch"

data Term = TermInf TermInf
          | TermCheck TermCheck
          deriving (Show, Eq)

eval :: Term -> Value
eval (TermInf x) = evalInf x []
eval (TermCheck x) = evalCheck x []

check :: [(Name, Value)] -> Term -> Value -> Either Text ()
check env (TermInf x) = typeCheck 0 env (Inf x)
check env (TermCheck x) = typeCheck 0 env x

testId :: TermCheck
testId = Lambda $ Lambda $ Inf $ Annotate (Inf $ Bound 0) (Inf $ Bound 1)

testBool :: TermInf
testBool = ADT "Bool" $ Sum [Product "True" [Inf Star], Product "False" []]

testFalse :: TermInf
testFalse = ADTConstruct "False" (Inf testBool) []

testElim :: TermInf
testElim = ADTEliminate testFalse (Inf Star) [("False", Inf testBool), ("True", Lambda $ Inf $ Bound 0)]
