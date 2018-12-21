module Bramble.Core.Named where

import GHC.Num ((+))

import Control.Applicative (pure, (<*>))
import Control.Monad (mapM)
import Control.Arrow (second)
import Control.Exception.Safe (MonadThrow, throwString)

import Data.Monoid (mconcat)
import Data.Functor ((<$>))
import Data.Function (($), (.))
import Data.Maybe (Maybe(..))
import Data.Eq (Eq, (==))
import Data.Bool (otherwise)
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

  NameEliminate :: NameTerm -> [(Text, NameTerm)] -> NameTerm
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
debruijn n i (NameEliminate x args) = NameEliminate (debruijn n i x) $ second (debruijn n i) <$> args

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
renameTerm (NameEliminate x args) = do
  rx <- renameTerm x
  case rx of
    TermCheck y -> throwString . unpack $ mconcat ["Cannot infer type of \"", pretty y, "\""]
    TermInf y -> TermInf <$> (ADTEliminate y <$> mapM (\(n, z) -> (n,) . repr <$> renameTerm z) args)

rename :: MonadThrow m => [Statement NameTerm] -> m [Statement Term]
rename = mapM (mapM renameTerm)
