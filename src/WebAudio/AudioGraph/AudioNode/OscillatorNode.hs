{-# LANGUAGE OverloadedStrings #-}

module WebAudio.AudioGraph.AudioNode.OscillatorNode where

import Data.Aeson (ToJSON (..), object, (.=))
import WebAudio.AudioGraph.AudioNode.Types
import WebAudio.AudioGraph.AudioParam

data OscWaveType = Sine | Square | Sawtooth | Triangle | Custom
  deriving (Eq)

instance Show OscWaveType where
  show Sine = "sine"
  show Square = "square"
  show Sawtooth = "sawtooth"
  show Triangle = "triangle"
  show Custom = "custom"

data OscillatorNode = OscillatorNode
  { oscFrequency :: AudioParam,
    oscDetune :: AudioParam,
    oscWaveType :: OscWaveType
  }
  deriving (Eq, Show)

instance AudioNodeBehavior OscillatorNode where
  updateAudioParamInNode oscNode newParam@(AudioParam (FrequencyParam, _)) = oscNode {oscFrequency = newParam}
  updateAudioParamInNode oscNode newParam@(AudioParam (DetuneParam, _)) = oscNode {oscDetune = newParam}
  updateAudioParamInNode _ _ = error "Invalid audio parameter for Oscillator node"

  extractParamFromAudioNode oscNode FrequencyParam = oscFrequency oscNode
  extractParamFromAudioNode oscNode DetuneParam = oscDetune oscNode
  extractParamFromAudioNode _ _ = error "Invalid audio parameter for Oscillator node"

  getParamsFromAudioNode oscNode =
    [ oscFrequency oscNode,
      oscDetune oscNode
    ]

  getAudioNodeType _ = "OscillatorNode"

instance ToJSON OscillatorNode where
  toJSON (OscillatorNode (AudioParam (FrequencyParam, frequencyValue)) (AudioParam (DetuneParam, detuneValue)) waveType) =
    object
      [ "frequency" .= frequencyValue,
        "detune" .= detuneValue,
        "type" .= show waveType -- this will map to  "type" in JSON
      ]

createOscillatorNode ::
  AudioParamValue -> -- frequency value
  AudioParamValue -> -- detune value
  OscWaveType -> -- wave type
  OscillatorNode
createOscillatorNode frequencyValue detuneValue waveType =
  OscillatorNode
    { oscFrequency = AudioParam (FrequencyParam, frequencyValue),
      oscDetune = AudioParam (DetuneParam, detuneValue),
      oscWaveType = waveType
    }
