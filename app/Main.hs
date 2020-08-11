module Main where

import Lambda.Format
import Lambda.Parser
import Lambda.Syntax
import Lambda.Calculation
import Text.Parsec
import System.Console.Haskeline
import Control.Monad.Trans ( liftIO )

main :: IO ()
main = do
  putStrLn "please enter your lambda item"
  lambda <- getLine
  putStrLn "do you want auto calculate or not?(yes/no)"
  option <- getLine
  calculate option lambda

calculate :: String -> String -> IO ()
calculate o l
  | o == "yes" || o == "y" = autoCalc lambda
  | otherwise = manualCalc lambda
  where autoCalc :: Either ParseError Lambda -> IO ()
        autoCalc (Left error) = print error >> main
        autoCalc (Right lambda') = (putStrLn . ("the result: " <>) . toString $ calculation lambda') >> main
        
        manualCalc :: Either ParseError Lambda -> IO ()
        manualCalc (Left error) = print error >> main
        manualCalc (Right lambda') = do
          putStrLn . ("current lambda: " <>) . toString $ lambda'
          putStrLn "please select you reduction?(α/β/η)"
          reduction <- getLine
          manualCalc' reduction lambda'

        manualCalc' :: String -> Lambda -> IO ()
        manualCalc' ('α':'-':xs) lambda = let a = αConversion xs lambda in continue a
        manualCalc' "β" lambda = let a = βReduction lambda in continue a
        manualCalc' "η" lambda = let a = ηReduction lambda in continue a

        continue :: Lambda -> IO ()
        continue lambda = do
          putStrLn . ("the result: " <>) . toString $ lambda
          putStrLn "is that ok?(yes/no)"
          option <- getLine
          if option == "yes" || option == "y"
            then main
            else manualCalc $ Right lambda

        lambda = parseLambda l

