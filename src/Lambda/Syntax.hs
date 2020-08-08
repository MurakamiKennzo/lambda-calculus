module Lambda.Syntax
  (
    Lambda(..)
  ) where

data Lambda = Variable String
            | Abstraction String Lambda
            | Application Lambda Lambda deriving (Show, Eq)
