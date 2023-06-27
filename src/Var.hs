module Var where

data NamedVar a = NamedVar {varName :: String, varValue :: a}

instance Show a => Show (NamedVar a) where
  show (NamedVar varName varValue) = varName ++ " = " ++ show varValue

instance Eq a => Eq (NamedVar a) where
  (NamedVar _ varValue1) == (NamedVar _ varValue2) = varValue1 == varValue2