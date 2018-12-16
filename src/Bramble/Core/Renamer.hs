module Bramble.Core.Renamer where

import GHC.Num ((+))

import Control.Applicative (pure, (<*>))
import Control.Monad (mapM)
import Control.Arrow (second)
import Control.Exception.Safe (MonadThrow, throwString)

import Data.Monoid (mconcat)
import Data.Functor (fmap, (<$>))
import Data.Function (($), (.))
import Data.Maybe (Maybe(..))
import Data.Eq (Eq, (==))
import Data.Bool (otherwise)
import Data.Int (Int)
import Data.Text (Text, unpack)

import Text.Show (Show)

import Bramble.Core.ADT
import Bramble.Core.AST

data NameTerm where
  NameAnnotate :: NameTerm -> NameTerm -> NameTerm
  NameStar :: NameTerm
  NamePi :: Maybe Text -> NameTerm -> NameTerm -> NameTerm
  NameFree :: Text -> NameTerm
  NameBound :: Int -> NameTerm
  NameApply :: NameTerm -> NameTerm -> NameTerm
  NameLambda :: Maybe Text -> NameTerm -> NameTerm

  NameADT :: Text -> Sum NameTerm -> NameTerm
  NameADTConstruct :: Text -> NameTerm -> [NameTerm] -> NameTerm
  NameADTEliminate :: NameTerm -> NameTerm -> [(Text, NameTerm)] -> NameTerm
deriving instance Show NameTerm
deriving instance Eq NameTerm

debruijn :: Maybe Text -> Int -> NameTerm -> NameTerm
debruijn n i (NameAnnotate e t) = NameAnnotate (debruijn n i e) $ debruijn n i t
debruijn _ _ NameStar = NameStar
debruijn n i (NamePi n' t b)
  | n == n' = NamePi n' (debruijn n i t) b
  | otherwise = NamePi n' (debruijn n i t) $ debruijn n (i + 1) b
debruijn n i a@(NameFree n')
  | n == Just n' = NameBound i
  | otherwise = a
debruijn _ _ x@NameBound{} = x
debruijn n i (NameApply f x) = NameApply (debruijn n i f) $ debruijn n i x
debruijn n i x@(NameLambda n' b)
  | n == n' = x
  | otherwise = NameLambda n' $ debruijn n (i + 1) b
debruijn _ _ x@NameADT{} = x
debruijn n i (NameADTConstruct n' t args) = NameADTConstruct n' (debruijn n i t) $ debruijn n i <$> args
debruijn n i (NameADTEliminate x t args) = NameADTEliminate (debruijn n i x) (debruijn n i t) $ second (debruijn n i) <$> args

rename :: MonadThrow m => NameTerm -> m Term
rename (NameAnnotate e t) = TermInf <$> (Annotate
                                         <$> (repr <$> rename e)
                                         <*> (repr <$> rename t))
rename NameStar = pure $ TermInf Star
rename (NamePi n t b) = TermInf <$> (Pi
                                     <$> (repr <$> rename t)
                                     <*> (repr <$> rename (debruijn n 0 b)))
rename (NameFree n) = pure . TermInf . Free $ Name n
rename (NameBound i) = pure . TermInf $ Bound i
rename (NameApply f x) = do
  rf <- rename f
  case rf of
    TermCheck y -> throwString . unpack $ mconcat ["Cannot infer type of \"", prettyCheck y, "\""]
    TermInf y -> TermInf <$> (Apply y <$> (repr <$> rename x))
rename (NameLambda n b) = TermCheck <$> (Lambda <$> (repr <$> rename (debruijn n 0 b)))
rename (NameADT n s) = TermInf <$> (ADT n <$> mapM (fmap repr . rename) s)
rename (NameADTConstruct n t args) = TermInf <$> (ADTConstruct n <$> (repr <$> rename t) <*> mapM (fmap repr . rename) args)
rename (NameADTEliminate x t args) = do
  rx <- rename x
  case rx of
    TermCheck y -> throwString . unpack $ mconcat ["Cannot infer type of \"", prettyCheck y, "\""]
    TermInf y -> TermInf <$> (ADTEliminate y <$> (repr <$> rename t) <*> mapM (\(n, z) -> (n,) . repr <$> rename z) args)
