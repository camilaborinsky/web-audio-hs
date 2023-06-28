module JavaScript where

import Var
import AudioParam

class JavaScript a where
  showJSInit :: a -> String

compileVariableInit :: JavaScript a => NamedVar a -> [Char]
compileVariableInit (NamedVar varName varValue) = "const " ++ varName ++ " = " ++ showJSInit varValue ++ ";\n"

compileAudioParam :: AudioParam -> String
compileAudioParam (Gain value) = "gain.value = " ++ show value
compileAudioParam (DelayTime value) = "delayTime.value = " ++ show value
compileAudioParam (Frequency value) = "frequency.value = " ++ show value
compileAudioParam (Q value) = "Q.value = " ++ show value
compileAudioParam (Detune value) = "detune.value = " ++ show value
compileAudioParam (Pan value) = "pan.value = " ++ show value

