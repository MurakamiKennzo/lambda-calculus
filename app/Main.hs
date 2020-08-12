module Main where

import Lambda.Format
import Lambda.Parser
import Lambda.Syntax
import Lambda.Calculation
import Text.Parsec
import System.Console.Haskeline
import Control.Monad.Trans ( liftIO )

repl :: InputT IO ()
repl = do
  outputStrLn "please enter your lambda item"
  (Just lambda) <- getInputLine "λ> "
  (Just option) <- getInputLine "do you want auto calculate or not?(yes/no) "
  calculate option lambda

main :: IO ()
main = runInputT defaultSettings repl

calculate :: String -> String -> InputT IO ()
calculate o l
  | o == "yes" || o == "y" = autoCalc lambda
  | otherwise = manualCalc lambda
  where autoCalc :: Either ParseError Lambda -> InputT IO ()
        autoCalc (Left error) = (outputStrLn . show  $ error) >> repl
        autoCalc (Right lambda') = (outputStrLn . ("the result: " <>) . toString $ calculation lambda') >> repl
        
        manualCalc :: Either ParseError Lambda -> InputT IO ()
        manualCalc (Left error) = (outputStrLn . show  $ error) >> repl
        manualCalc (Right lambda') = do
          outputStrLn . ("current lambda: " <>) . toString $ lambda'
          (Just reduction) <- getInputLine "please select you reduction?(α/β/η/quit) "
          manualCalc' reduction lambda'

        manualCalc' :: String -> Lambda -> InputT IO ()
        manualCalc' ('α':'-':xs) lambda = let a = αConversion xs lambda in continue a
        manualCalc' "β" lambda = let a = βReduction lambda in continue a
        manualCalc' "η" lambda = let a = ηReduction lambda in continue a
        manualCalc' "quit" lambda = repl

        continue :: Lambda -> InputT IO ()
        continue lambda = manualCalc $ Right lambda

        lambda = parseLambda l
