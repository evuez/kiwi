module Main where

import Control.Monad.Except (runExceptT)
import Lib (App(..), AppError(..), readConfig, run)

main :: IO ()
main = do
  c <- (runExceptT . runApp . readConfig) "kiwi.toml"
  case c of
    Left e -> renderError e
    Right c' -> do
      putStrLn "Using config from kiwi.toml:\n"
      either renderError return =<< (runExceptT . runApp . run) c'

renderError :: AppError -> IO ()
renderError (TemplateError e) = do
  putStrLn "Template error:\n"
  print e
renderError (ConfigError e) = do
  putStrLn "Config error:\n"
  print e
