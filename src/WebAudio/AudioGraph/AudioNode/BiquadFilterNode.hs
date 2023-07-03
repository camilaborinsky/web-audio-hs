{-# LANGUAGE OverloadedStrings #-}

module WebAudio.AudioGraph.AudioNode.BiquadFilterNode where

import Data.Aeson (ToJSON (..), object, (.=))
import WebAudio.AudioGraph.AudioNode.Types
import WebAudio.AudioGraph.AudioParam

data BiquadFilterType = Lowpass | Highpass | Bandpass | Lowshelf | Highshelf | Peaking | Notch | Allpass
  deriving (Eq)

instance Show BiquadFilterType where
  show Lowpass = "lowpass"
  show Highpass = "highpass"
  show Bandpass = "bandpass"
  show Lowshelf = "lowshelf"
  show Highshelf = "highshelf"
  show Peaking = "peaking"
  show Notch = "notch"
  show Allpass = "allpass"

data BiquadFilterNode = BiquadFilterNode
  { biquadFrequency :: AudioParam,
    biquadDetune :: AudioParam,
    biquadQParam :: AudioParam,
    biquadGain :: AudioParam,
    biquadFilterType :: BiquadFilterType
  }
  deriving (Eq, Show)

instance AudioNodeBehavior BiquadFilterNode where
  updateAudioParamInNode biquadFilterNode newParam@(AudioParam (FrequencyParam, _)) = biquadFilterNode {biquadFrequency = newParam}
  updateAudioParamInNode biquadFilterNode newParam@(AudioParam (DetuneParam, _)) = biquadFilterNode {biquadDetune = newParam}
  updateAudioParamInNode biquadFilterNode newParam@(AudioParam (QParam, _)) = biquadFilterNode {biquadQParam = newParam}
  updateAudioParamInNode biquadFilterNode newParam@(AudioParam (GainParam, _)) = biquadFilterNode {biquadGain = newParam}
  updateAudioParamInNode _ _ = error "Invalid audio parameter for BiquadFilter node" 

  extractParamFromAudioNode biquadFilterNode FrequencyParam = biquadFrequency biquadFilterNode
  extractParamFromAudioNode biquadFilterNode DetuneParam = biquadDetune biquadFilterNode
  extractParamFromAudioNode biquadFilterNode QParam = biquadQParam biquadFilterNode
  extractParamFromAudioNode biquadFilterNode GainParam = biquadGain biquadFilterNode
  extractParamFromAudioNode _ _ = error "Invalid audio parameter for BiquadFilter node"

  getParamsFromAudioNode biquadFilterNode =
    [ biquadFrequency biquadFilterNode,
      biquadDetune biquadFilterNode,
      biquadQParam biquadFilterNode,
      biquadGain biquadFilterNode
    ]

  getAudioNodeType _ = "BiquadFilterNode"

createBiquadFilterNode ::
  AudioParamValue -> -- frequency value
  AudioParamValue -> -- detune value
  AudioParamValue -> -- Q value
  AudioParamValue -> -- gain value
  BiquadFilterType -> -- filter type
  AudioNode
createBiquadFilterNode frequencyValue detuneValue qValue gainValue filterType =
  AudioNode BiquadFilterNode
    { biquadFrequency = AudioParam (FrequencyParam, frequencyValue),
      biquadDetune = AudioParam (DetuneParam, detuneValue),
      biquadQParam = AudioParam (QParam, qValue),
      biquadGain = AudioParam (GainParam, gainValue),
      biquadFilterType = filterType
    }

instance ToJSON BiquadFilterNode where
  toJSON (BiquadFilterNode (AudioParam (FrequencyParam, frequencyValue)) (AudioParam (QParam, qValue)) (AudioParam (GainParam, gainValue)) (AudioParam (DetuneParam, detuneValue)) filterType) =
    object
      [ "frequency" .= frequencyValue,
        "Q" .= qValue,
        "gain" .= gainValue,
        "detune" .= detuneValue,
        "type" .= show filterType
      ]

-- updateAudioParamInBiquadFilterNode :: BiquadFilterNode -> AudioParam -> BiquadFilterNode
-- updateAudioParamInBiquadFilterNode biquadFilterNode newParam@(AudioParam (FrequencyParam, _)) = biquadFilterNode {biquadFrequency = newParam}
-- updateAudioParamInBiquadFilterNode biquadFilterNode newParam@(AudioParam (DetuneParam, _)) = biquadFilterNode {biquadDetune = newParam}
-- updateAudioParamInBiquadFilterNode biquadFilterNode newParam@(AudioParam (QParam, _)) = biquadFilterNode {biquadQParam = newParam}
-- updateAudioParamInBiquadFilterNode biquadFilterNode newParam@(AudioParam (GainParam, _)) = biquadFilterNode {biquadGain = newParam}
-- updateAudioParamInBiquadFilterNode _ _ = error "Invalid audio parameter for BiquadFilter node"

-- extractParamFromBiQuadFilterNode :: BiquadFilterNode -> AudioParamType -> AudioParam
-- extractParamFromBiQuadFilterNode biquadFilterNode FrequencyParam = biquadFrequency biquadFilterNode
-- extractParamFromBiQuadFilterNode biquadFilterNode DetuneParam = biquadDetune biquadFilterNode
-- extractParamFromBiQuadFilterNode biquadFilterNode QParam = biquadQParam biquadFilterNode
-- extractParamFromBiQuadFilterNode biquadFilterNode GainParam = biquadGain biquadFilterNode
-- extractParamFromBiQuadFilterNode _ _ = error "Invalid audio parameter for BiquadFilter node"