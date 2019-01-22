module Bramble.Core.Named where

import GHC.Num ((+))

import Control.Applicative (pure, (<*>))
import Control.Monad (mapM)
import Control.Arrow (second)
import Control.Exception.Safe (MonadThrow, throwString)

import Data.Monoid (mconcat)
import Data.Functor ((<$>))
import Data.Function (flip, ($), (.))
import Data.Maybe (Maybe(..))
import Data.Eq (Eq, (==))
import Data.Bool (Bool, otherwise)
import Data.Int (Int)
import Data.Text (Text, unpack)

import Text.Show (Show)

import Bramble.Utility.Pretty
import Bramble.Core.Calculus
import Bramble.Core.Vernacular

data NameTerm where
  NameAnnotate :: NameTerm -> NameTerm -> NameTerm
  NameStar :: NameTerm
  NamePi :: Maybe Text -> NameTerm -> NameTerm -> NameTerm
  NameFree :: Text -> NameTerm
  NameBound :: Int -> NameTerm
  NameApply :: NameTerm -> NameTerm -> NameTerm
  NameLambda :: Maybe Text -> NameTerm -> NameTerm

  NameADTEliminate :: NameTerm -> [(Text, NameTerm)] -> NameTerm
  NameRow :: Bool -> [(Text, NameTerm)] -> [Text] -> NameTerm
  NameRowConstruct :: [(Text, NameTerm)] -> NameTerm
  NameRowEliminate :: NameTerm -> Text -> NameTerm
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
debruijn n i (NameADTEliminate x args) = NameADTEliminate (debruijn n i x) $ second (debruijn n i) <$> args
debruijn n i (NameRow e fs es) = NameRow e (second (debruijn n i) <$> fs) es
debruijn n i (NameRowConstruct fs) = NameRowConstruct $ second (debruijn n i) <$> fs
debruijn n i (NameRowEliminate x fn) = NameRowEliminate (debruijn n i x) fn

renameTerm :: MonadThrow m => NameTerm -> m Term
renameTerm (NameAnnotate e t) = TermInf <$> (Annotate
                                         <$> (repr <$> renameTerm e)
                                         <*> (repr <$> renameTerm t))
renameTerm NameStar = pure $ TermInf Star
renameTerm (NamePi n t b) = TermInf <$> (Pi
                                     <$> (repr <$> renameTerm t)
                                     <*> (repr <$> renameTerm (debruijn n 0 b)))
renameTerm (NameFree n) = pure . TermInf . Free $ Name n
renameTerm (NameBound i) = pure . TermInf $ Bound i
renameTerm (NameApply f x) = do
  rf <- renameTerm f
  case rf of
    TermCheck y -> throwString . unpack $ mconcat ["Cannot infer type of \"", pretty y, "\""]
    TermInf y -> TermInf <$> (Apply y <$> (repr <$> renameTerm x))
renameTerm (NameLambda n b) = TermCheck <$> (Lambda <$> (repr <$> renameTerm (debruijn n 0 b)))
renameTerm (NameADTEliminate x args) = do
  rx <- renameTerm x
  case rx of
    TermCheck y -> throwString . unpack $ mconcat ["Cannot infer type of \"", pretty y, "\""]
    TermInf y -> TermInf <$> (ADTEliminate y <$> mapM (\(n, z) -> (n,) . repr <$> renameTerm z) args)
renameTerm (NameRow e fs es) = TermInf . flip (Row e) es <$> mapM (\(n, x) -> (n,) . repr <$> renameTerm x) fs
renameTerm (NameRowConstruct fs) = TermCheck . RowConstruct <$> mapM (\(n, x) -> (n,) . repr <$> renameTerm x) fs
renameTerm (NameRowEliminate x fn) = do
  rx <- renameTerm x
  case rx of
    TermCheck y -> throwString . unpack $ mconcat ["Cannot infer type of \"", pretty y, "\""]
    TermInf y -> pure . TermInf $ RowEliminate y fn

rename :: MonadThrow m => [Statement NameTerm] -> m [Statement Term]
rename = mapM (mapM renameTerm)
