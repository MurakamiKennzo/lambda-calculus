module Lambda.Calculation 
  (
    calculation
  , βReduction
  ) where

import Lambda.Syntax
import Data.List ( delete )

variables :: Lambda -> [String]
variables (Variable a) = [a]
variables (Abstraction s lambda) = s: variables lambda
variables (Application lambda lambda') = variables lambda <> variables lambda'

freeVariables :: Lambda -> [String]
freeVariables (Variable a) = [a]
freeVariables (Abstraction s lambda) = delete s $ freeVariables lambda
freeVariables (Application lambda lambda') = freeVariables lambda <> freeVariables lambda'

captureAvoidingSubsitution :: String -> Lambda -> Lambda -> Lambda
captureAvoidingSubsitution x replacement (Variable y)
  | x == y = replacement
  | otherwise = Variable y
captureAvoidingSubsitution replace replacement (Application lambda lambda') = Application lambda'' lambda'''
  where lambda'' = captureAvoidingSubsitution replace replacement lambda
        lambda''' = captureAvoidingSubsitution replace replacement lambda'
captureAvoidingSubsitution x replacement (Abstraction y lambda)
  | x /= y && y `notElem` freeVariables replacement = Abstraction y lambda'
  | otherwise = Abstraction y lambda
  where lambda' = captureAvoidingSubsitution x replacement lambda

αConversion :: String -> Lambda -> Lambda
αConversion x (Abstraction y lambda)
  | x `notElem` variables lambda = Abstraction x lambda'
  | otherwise = Abstraction y lambda
  where lambda' = captureAvoidingSubsitution y (Variable x) lambda

βReduction :: Lambda -> Lambda
βReduction (Application (Abstraction s lambda) lambda') = captureAvoidingSubsitution s lambda' lambda
βReduction (Application a@(Application _ _) lambda) = βReduction $ Application (βReduction a) lambda
βReduction lambda = lambda

ηReduction :: Lambda -> Lambda
ηReduction a@(Abstraction x (Application lambda (Variable y)))
  | x == y && x `notElem` freeVariables lambda = lambda
  | otherwise = a

calculation :: Lambda -> Lambda
calculation lambda = if lambda == lambda' then lambda else calculation lambda'
  where lambda' = βReduction lambda
