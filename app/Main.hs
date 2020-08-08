module Main where

import Lambda.Format
import Lambda.Parser
import Lambda.Calculation
import System.Console.Haskeline
import Control.Monad.Trans ( liftIO )

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "λ> "
    case minput of
      Nothing -> liftIO main
      Just "quit" -> return ()
      Just input -> (>> loop) $ either (return $ outputStrLn "Can't parse or calclate lambda.") (outputStrLn . toString . calculation) $ parseLambda input
