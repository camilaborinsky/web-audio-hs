module AudioContext where

import JavaScript
import Var

data AudioContext = AudioContext
  deriving (Eq)

globalAudioContext = NamedVar {varName = "audioContext", varValue = AudioContext}

instance JavaScript AudioContext where
  showJSInit AudioContext = "new AudioContext()"