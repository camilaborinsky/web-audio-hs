{-# LANGUAGE ImportQualifiedPost #-}

module JS.Compiler where

import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Var
import WebAudio.AudioGraph.AudioNode.Types
import WebAudio.AudioGraph.AudioParam
import WebAudio.AudioGraph.AudioContext

class ToJSON a => JavaScript a where
  showJSInit :: a -> String
  showJSConstructorName :: a -> String

compileVariableInit :: JavaScript a => NamedVar a -> [Char]
compileVariableInit (NamedVar varName varValue) = "const " ++ varName ++ " = " ++ showJSInit varValue ++ ";\n"

compileSetAudioParam :: AudioNodeVar -> AudioParam -> String
compileSetAudioParam audioNodeVar (AudioParam (paramType, paramValue)) =
  varName audioNodeVar ++ "." ++ compileAudioParamField paramType ++ ".value = " ++ show paramValue ++ ";\n"

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

instance JavaScript AudioContext where
  showJSConstructorName AudioContext = "AudioContext"
  showJSInit AudioContext = "new " ++ showJSConstructorName AudioContext ++ "()"

instance JavaScript AudioNode where
  showJSConstructorName (AudioNode node) = getAudioNodeType node

  showJSInit audioNode =
    let jsonParams = unpack . decodeUtf8 . BL.toStrict $ encode audioNode
     in "new " ++ showJSConstructorName audioNode ++ "(" ++ varName globalAudioContext ++ ", " ++ jsonParams ++ ")"