module Main where

import Parser

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process = either onErr onExpr . parseToplevel
  where
    onErr = print
    onExpr = (mapM_ print)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> (liftIO $ process input) >> loop
