module Main where

import Control.Applicative (pure, (<**>))
import Control.Monad ((>>=))
import Control.Monad.IO.Class (liftIO)
import Control.Exception.Safe (Handler(..), catches, displayException)

import Data.Monoid (mconcat, (<>))
import Data.Functor (fmap, (<$>))
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Text (pack)

import System.IO (IO, putStrLn)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine)

import Options.Applicative (Parser, execParser, subparser, command, info, helper, progDesc, fullDesc, header)

import Bramble.Utility.Error
import Bramble.Frontend
import Bramble.Core.Calculus
import Bramble.Backend.Interpreter

data ReplOptions = ReplOptions

replOptions :: Parser ReplOptions
replOptions = pure ReplOptions

newtype Options = Repl ReplOptions

options :: Parser Options
options = subparser $ mconcat
  [ command "repl" (info (Repl <$> replOptions) (progDesc "Launch REPL"))
  ]

run :: Options -> IO ()
run Repl{} = runInputT defaultSettings $ loop []
  where loop :: [(Name, Value, Value)] -> InputT IO ()
        loop env = do
          l <- fmap pack <$> getInputLine "λ "
          case l of
            Nothing -> pure ()
            Just line -> do
              env' <- liftIO $ catches (frontend "input" line >>= interpret env)
                [ Handler $ \(e :: TypeError) -> do
                    putStrLn $ "Type error: " <> displayException e
                    pure env
                , Handler $ \(e :: ParseError) -> do
                    putStrLn $ "Parse error: " <> displayException e
                    pure env
                , Handler $ \(e :: VernacularError) -> do
                    putStrLn $ "Read error: " <> displayException e
                    pure env
                ]
              loop env'

main :: IO ()
main = execParser opts >>= run
  where opts = info (options <**> helper) $ mconcat
          [ fullDesc
          , header "bramble - Compiler for Bramble Lisp"
          ]
