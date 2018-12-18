module Bramble.Utility.Error where

import Control.Exception.Safe (Exception, displayException)

import Data.Monoid (mconcat)
import Data.Function (($))
import Data.Int (Int)
import Data.Text (Text, pack, unpack)

import Text.Show (Show, show)

data TypeError
  = UnboundIndex Int
  | UnknownIdentifier Text
  | IllegalApplication Text Text Text
  | ConstructorArityMismatch Text Int Int
  | InvalidConstructor Text Text
  | ConstructNonADT Text Text
  | MissingCases Text Int Int
  | EliminateNonADT Text Text
  | TypeMismatch Text Text Text
  | StructuralTypeMismatch Text Text
  | CannotInferType Text
  deriving Show

instance Exception TypeError where
  displayException (UnboundIndex i) = unpack $ mconcat ["Unbound variable with index \"", pack $ show i, "\""]
  displayException (UnknownIdentifier n) = unpack $ mconcat ["Unknown identifier \"", n, "\""]
  displayException (IllegalApplication f t x) = unpack $ mconcat
    [ "Illegal application of \"", f
    , "\" (with type \"", t
    , "\") to \"", x, "\""
    ]
  displayException (ConstructorArityMismatch cn i j) = unpack $ mconcat
    [ "Constructor arity mismatch for \"", cn
    , "\": expected ", pack $ show i
    , " but received ", pack $ show j
    ]
  displayException (InvalidConstructor t cn) = unpack $ mconcat
    [ "The ADT \"", t
    , "\" does not have the constructor \"", cn, "\""
    ]
  displayException (ConstructNonADT t cn) = unpack $ mconcat
    [ "Attempt to construct non-ADT type \"", t
    , "\" using constructor \"", cn, "\""
    ]
  displayException (MissingCases t i j) = unpack $ mconcat
    [ "Not enough cases to eliminate \"", t
    , "\": expected ", pack $ show i
    , " but received ", pack $ show j
    ]
  displayException (EliminateNonADT x t) = unpack $ mconcat
    [ "Attempt to eliminate term \"", x
    , "\" of non-ADT type \"", t, "\""
    ]
  displayException (TypeMismatch t x t') = unpack $ mconcat
    [ "Expected \"", t
    , "\" but found \"", x
    , "\" with type \"", t', "\""
    ]
  displayException (StructuralTypeMismatch t x) = unpack $ mconcat
    [ "Expected \"", t
    , "\" but found \"", x, "\""
    ]
  displayException (CannotInferType x) = unpack $ mconcat ["Cannot infer type of \"", x, "\""]

newtype ParseError
  = ParseError Text
  deriving Show

instance Exception ParseError where
  displayException (ParseError e) = unpack e

data VernacularError
  = VernacularStatementError Text
  | VernacularExpressionError Text
  | VernacularConstructorError Text
  | VernacularLambdaBinderError Text
  | VernacularLambdaBinderListError Text
  | VernacularPiBinderError Text
  | VernacularPiBinderListError Text
  deriving Show

instance Exception VernacularError where
  displayException (VernacularStatementError exp) = unpack $ mconcat ["Could not convert S-expression \"", exp, "\" to a vernacular statement"]
  displayException (VernacularExpressionError exp) = unpack $ mconcat ["Could not convert S-expression \"", exp, "\" to a vernacular expression"]
  displayException (VernacularConstructorError exp) = unpack $ mconcat ["Could not convert S-expression \"", exp, "\" to a vernacular constructor definition"]
  displayException (VernacularLambdaBinderError exp) = unpack $ mconcat ["Could not convert S-expression \"", exp, "\" to a vernacular lambda-binder"]
  displayException (VernacularLambdaBinderListError exp) = unpack $ mconcat ["Could not convert S-expression \"", exp, "\" to a list of vernacular lambda-binders"]
  displayException (VernacularPiBinderError exp) = unpack $ mconcat ["Could not convert S-expression \"", exp, "\" to a vernacular pi-binder"]
  displayException (VernacularPiBinderListError exp) = unpack $ mconcat ["Could not convert S-expression \"", exp, "\" to a list of vernacular pi-binders"]
