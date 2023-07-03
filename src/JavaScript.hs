module JavaScript where

import AudioNode (AudioNodeVar)
import AudioParam
import Var

class JavaScript a where
  showJSInit :: a -> String

-- TODO: Reubicar funciones propias de nodes a WebAudioApiJS.hs
compileSetAudioParam :: AudioNodeVar -> AudioParam -> String
compileSetAudioParam audioNodeVar (AudioParam (paramType, paramValue)) =
  varName audioNodeVar ++ "." ++ compileAudioParamField paramType ++ ".value = " ++ show paramValue ++ ";\n"

compileVariableInit :: JavaScript a => NamedVar a -> [Char]
compileVariableInit (NamedVar varName varValue) = "const " ++ varName ++ " = " ++ showJSInit varValue ++ ";\n"

compileAudioParamField :: AudioParamType -> String
compileAudioParamField GainParam = "gain"
compileAudioParamField DelayTimeParam = "delayTime"
compileAudioParamField FrequencyParam = "frequency"
compileAudioParamField QParam = "Q"
compileAudioParamField DetuneParam = "detune"
compileAudioParamField PanParam = "pan"

compileGetAudioParam :: AudioNodeVar -> AudioParamType -> String -> String
compileGetAudioParam audioNodeVar paramType paramVarName =
  let paramField = compileAudioParamField paramType
   in "const " ++ paramVarName ++ " = " ++ varName audioNodeVar ++ "." ++ paramField ++ ";\n"
