module WebAudio.Types.OscillatorNode where

import WebAudio.Types.AudioParam

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

updateAudioParamInOscillatorNode :: OscillatorNode -> AudioParam -> OscillatorNode
updateAudioParamInOscillatorNode oscNode newParam@(AudioParam (FrequencyParam, _)) = oscNode {oscFrequency = newParam}
updateAudioParamInOscillatorNode oscNode newParam@(AudioParam (DetuneParam, _)) = oscNode {oscDetune = newParam}
updateAudioParamInOscillatorNode _ _ = error "Invalid audio parameter for given OscillatorNode"

extractParamFromOscillatorNode :: OscillatorNode -> AudioParamType-> AudioParam
extractParamFromOscillatorNode oscNode FrequencyParam = oscFrequency oscNode
extractParamFromOscillatorNode oscNode DetuneParam = oscDetune oscNode
extractParamFromOscillatorNode _ _ = error "Invalid audio parameter for given OscillatorNode"

getParamsFromOscillatorNode :: OscillatorNode -> [AudioParam]
getParamsFromOscillatorNode oscNode = [oscFrequency oscNode, oscDetune oscNode]