module WebAudio.AudioGraph.AudioContext where

import Data.Aeson

import Var

data AudioContext = AudioContext
  deriving (Eq)

globalAudioContext = NamedVar {varName = "audioContext", varValue = AudioContext}

instance ToJSON AudioContext where
  toJSON _ = object []