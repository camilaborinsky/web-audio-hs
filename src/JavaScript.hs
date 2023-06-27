module JavaScript where

import Var

class JavaScript a where
  showJSInit :: a -> String

compileVariableInit :: JavaScript a => NamedVar a -> [Char]
compileVariableInit (NamedVar varName varValue) = "const " ++ varName ++ " = " ++ showJSInit varValue ++ ";\n"