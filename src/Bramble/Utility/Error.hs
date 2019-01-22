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
  | IllegalFixpoint Text Text
  | ConstructorArityMismatch Text Int Int
  | InvalidConstructor Text Text
  | ConstructNonADT Text Text
  | MissingCases Text Int Int
  | BadCase Text Text
  | EmptyCase
  | EliminateNonADT Text Text
  | TypeMismatch Text Text Text
  | StructuralTypeMismatch Text Text
  | CannotInferType Text
  | ImpossibleRow Text Text
  | EliminateNonRow Text Text Text
  | MissingField Text Text Text
  | ExcludedField Text Text Text
  deriving Show

instance Exception TypeError where
  displayException (UnboundIndex i) = unpack $ mconcat ["Unbound variable with index \"", pack $ show i, "\""]
  displayException (UnknownIdentifier n) = unpack $ mconcat ["Unknown identifier \"", n, "\""]
  displayException (IllegalApplication f t x) = unpack $ mconcat
    [ "Illegal application of \"", f
    , "\" (with type \"", t
    , "\") to \"", x, "\""
    ]
  displayException (IllegalFixpoint f t) = unpack $ mconcat
    [ "Illegal fixpoint of \"", f
    , "\" (with type \"", t , "\")"
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
  displayException (BadCase x elim) = unpack $ mconcat ["Bad case branch in \"", elim, "\" when attempting to eliminate \"", x, "\""]
  displayException EmptyCase = "Empty case expression is disallowed"
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
  displayException (ImpossibleRow fn r) = unpack $ mconcat ["Row \"", r, "\" both requires and excludes field \"", fn, "\""]
  displayException (EliminateNonRow fn x t) = unpack $ mconcat
    [ "Attempt to access field \"", fn
    , "\" of term \"", x
    , "\" with non-row type \"", t, "\""
    ]
  displayException (MissingField fn x t) = unpack $ mconcat
    [ "Term \"", x
    , "\" of type \"", t
    , "\" does not have field \"", fn, "\""
    ]
  displayException (ExcludedField fn x t) = unpack $ mconcat
    [ "Term \"", x
    , "\" of type \"", t
    , "\" has excluded field \"", fn, "\""
    ]

newtype ParseError
  = ParseError Text
  deriving Show

instance Exception ParseError where
  displayException (ParseError e) = unpack e

data ReadError
  = ReadStatementError Text
  | ReadExpressionError Text
  | ReadConstructorError Text
  | ReadLambdaBinderError Text
  | ReadLambdaBinderListError Text
  | ReadPiBinderError Text
  | ReadPiBinderListError Text
  | ReadParameterError Text
  | ReadCaseError Text
  | ReadRowBinderError Text
  deriving Show

instance Exception ReadError where
  displayException (ReadStatementError exp) = unpack $ mconcat ["Could not read S-expression \"", exp, "\" as a vernacular statement"]
  displayException (ReadExpressionError exp) = unpack $ mconcat ["Could not read S-expression \"", exp, "\" as an expression"]
  displayException (ReadConstructorError exp) = unpack $ mconcat ["Could not read S-expression \"", exp, "\" as a constructor definition"]
  displayException (ReadLambdaBinderError exp) = unpack $ mconcat ["Could not read S-expression \"", exp, "\" as a function parameter"]
  displayException (ReadLambdaBinderListError exp) = unpack $ mconcat ["Could not read S-expression \"", exp, "\" as a list of function parameters"]
  displayException (ReadPiBinderError exp) = unpack $ mconcat ["Could not read S-expression \"", exp, "\" as a binder"]
  displayException (ReadPiBinderListError exp) = unpack $ mconcat ["Could not read S-expression \"", exp, "\" as a list of binders"]
  displayException (ReadParameterError exp) = unpack $ mconcat ["Could not read S-expression \"", exp, "\" as a data type parameter"]
  displayException (ReadCaseError exp) = unpack $ mconcat ["Could not read S-expression \"", exp, "\" as a case expression"]
  displayException (ReadRowBinderError exp) = unpack $ mconcat ["Could not read S-expression \"", exp, "\" as a row binder"]
