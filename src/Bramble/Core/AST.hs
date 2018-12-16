module Bramble.Core.AST where

import Prelude

import Control.Monad (forM_, unless)
import Control.Arrow (second)
import Control.Exception.Safe

import Data.Text (Text)
import qualified Data.Text as T

import Bramble.Utility.Pretty
import Bramble.Core.ADT

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
  pretty (Quote i) = mconcat ["<quote-", T.pack $ show i, ">"]
  pretty (Local i) = mconcat ["<local-", T.pack $ show i, ">"]

data TermInf where
  Annotate :: TermCheck -> TermCheck -> TermInf
  Star :: TermInf
  Pi :: TermCheck -> TermCheck -> TermInf
  Bound :: Int -> TermInf
  Free :: Name -> TermInf
  Apply :: TermInf -> TermCheck -> TermInf

  ADT :: Text -> Sum TermCheck -> TermInf
  ADTConstruct :: Text -> TermCheck -> [TermCheck] -> TermInf
  ADTEliminate :: TermInf -> TermCheck -> [(Text, TermCheck)] -> TermInf
deriving instance Show TermInf
deriving instance Eq TermInf

instance Pretty TermInf where
  pretty (Annotate e t) = mconcat ["[", pretty e, " : ", pretty t, "]"]
  pretty Star = "Type"
  pretty (Pi t b) = mconcat ["Π", pretty t, ".", pretty b]
  pretty (Bound i) = T.pack $ show i
  pretty (Free n) = pretty n
  pretty (Apply f x) = mconcat ["(", pretty f, " ", pretty x, ")"]
  pretty (ADT n s) = mconcat ["{", n, " = ", pretty s, "}"]
  pretty (ADTConstruct cn _ args) = "{" <> cn <> " " <> T.intercalate " " (pretty <$> args) <> "}"
  pretty (ADTEliminate x _ hs) = "{" <> pretty x <> " ! " <> T.intercalate "; " ((\(cn, b) -> cn <> " -> " <> pretty b) <$> hs) <> "}"

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
  NADTEliminate :: Neutral -> Neutral -> [(Text, Value)] -> Neutral

data Value where
  VLambda :: (Value -> Value) -> Value
  VStar :: Value
  VPi :: Value -> (Value -> Value) -> Value
  VNeutral :: Neutral -> Value

  VADT :: Text -> Sum TermCheck -> Value
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
        go _ (VADT n s) = Inf $ ADT n s
        go i (VADTConstruct n t args) = Inf . ADTConstruct n (go i t) $ go i <$> args
        nq :: Int -> Neutral -> TermInf
        nq i (NFree x) = boundfree i x
        nq i (NApply n v) = Apply (nq i n) $ go i v
        nq i (NADTEliminate n t hs) = ADTEliminate (nq i n) (Inf $ nq i t) $ second (go i) <$> hs
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
typeInf _ _ (Bound i) = throwString $ mconcat ["Unbound variable with index \"", show i, "\""]
typeInf _ env (Free n) = case lookup n env of
  Just t -> pure t
  Nothing -> throwString $ mconcat ["Unknown identifier \"", show n, "\""]
typeInf i env (Apply f x) = do
  ft <- typeInf i env f
  case ft of
    VPi t b -> do
      typeCheck i env x t
      pure . b $ evalCheck x []
    _ -> throwString $ mconcat
      [ "Illegal application of \"", T.unpack $ pretty f
      , "\" (with type \"", T.unpack . pretty $ quote ft
      , "\") to \"", T.unpack $ pretty x, "\""
      ]
typeInf i env (ADT _ s) = do
  validateSum i env s
  pure VStar
typeInf i env (ADTConstruct cn t args) = case evalCheck t [] of
  t'@(VADT n s) ->
    case lookupProduct cn s of
      Just (Product _ fs) ->
        if length args == length fs
        then do forM_ (zip fs args) $ \(ft, a) -> do
                  let ft' = evalCheck ft []
                  typeCheck i env a ft'
                pure t'
        else throwString $ mconcat
             [ "Constructor arity mismatch for \"", T.unpack cn
             , "\": expected ", show $ length fs
             , " but received ", show $ length args
             ]
      Nothing -> throwString $ mconcat
                 [ "The ADT \"", T.unpack n
                 , "\" does not have the constructor \"", T.unpack cn, "\""
                 ]
  _ -> throwString $ mconcat
    [ "Attempt to construct non-ADT type \"", T.unpack $ pretty t
    , "\" using constructor \"", T.unpack cn, "\""
    ]
typeInf i env (ADTEliminate x t hs) = do
  xt <- typeInf i env x
  let t' = evalCheck t []
  typeCheck i env t VStar
  case xt of
    VADT n s@(Sum ps) ->
      if length ps == length hs
      then do
        forM_ hs $ \(cn, h) ->
          case lookupProduct cn s of
            Just (Product _ p) ->
              typeCheck i env h $ foldr (\pt b -> VPi pt $ const b) t' $ flip evalCheck [] <$> p
            _ -> throwString $ mconcat
              [ "The ADT \"", T.unpack n
              , "\" does not have the constructor \"", T.unpack cn, "\""
              ]
        pure . VPi xt $ const t'
      else throwString $ mconcat
           [ "Not enough cases to eliminate \"", T.unpack n
           , "\": expected ", show $ length ps
           , " but received ", show $ length hs
           ]
    _ -> throwString $ mconcat
         [ "Attempt to eliminate term \"", T.unpack $ pretty x
         , "\" of non-ADT type \"", T.unpack . pretty $ quote xt, "\""
         ]
typeCheck :: MonadThrow m => Int -> [(Name, Value)] -> TermCheck -> Value -> m ()
typeCheck i env (Inf x) v = do
  v' <- typeInf i env x
  unless (quote v == quote v') . throwString $ mconcat
    [ "Type mismatch: expected \"", T.unpack . pretty $ quote v
    , "\" but found \"", T.unpack $ pretty x
    , "\" with type \"", T.unpack . pretty $ quote v', "\""
    ]
typeCheck i env (Lambda b) (VPi t b') =
  typeCheck (i + 1) ((Local i, t):env) (substCheck 0 (Free (Local i)) b) . b' . VNeutral . NFree $ Local i
typeCheck _ _ x v = throwString $ mconcat
  [ "Structural type mismatch: expected \"", T.unpack . pretty $ quote v
  , "\" but found \"", T.unpack $ pretty x, "\""
  ]

data Term = TermInf TermInf
          | TermCheck TermCheck
          deriving (Show, Eq)

instance Pretty Term where
  pretty = pretty . repr

repr :: Term -> TermCheck
repr (TermInf x) = Inf x
repr (TermCheck x) = x

eval :: Term -> Value
eval (TermInf x) = evalInf x []
eval (TermCheck x) = evalCheck x []

check :: MonadThrow m => [(Name, Value)] -> Term -> Value -> m ()
check env x = typeCheck 0 env $ repr x

testId :: TermCheck
testId = Lambda $ Lambda $ Inf $ Annotate (Inf $ Bound 0) (Inf $ Bound 1)

testBool :: TermInf
testBool = ADT "Bool" $ Sum [Product "True" [], Product "False" []]

testFalse :: TermInf
testFalse = ADTConstruct "False" (Inf testBool) []

testElim :: TermInf
testElim = ADTEliminate testFalse (Inf Star) [("False", Inf testBool), ("True", Inf Star)]
