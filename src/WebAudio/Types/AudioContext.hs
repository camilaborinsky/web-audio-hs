module WebAudio.Types.AudioContext where

import Var

data AudioContext = AudioContext
  deriving (Eq)

globalAudioContext = NamedVar {varName = "audioContext", varValue = AudioContext}