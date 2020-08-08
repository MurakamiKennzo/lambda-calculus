module Lambda.Format
  (
    toString
  ) where

import Lambda.Syntax

toString :: Lambda -> String
toString (Variable a) = a
toString (Abstraction s lambda) = "λ " <> s <> ". " <> toString lambda
toString (Application (Variable a) (Variable b)) = a <> " " <> b
toString (Application (Variable a) lambda) = a <> " (" <> toString lambda <> ")"
toString (Application lambda (Variable a)) = "(" <> toString lambda <> ") " <> a 
toString (Application lambda lambda') = "(" <> toString lambda <> ") (" <> toString lambda' <> ")" 
