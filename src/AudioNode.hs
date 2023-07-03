module AudioNode where

import AudioParam
import Var (NamedVar (..))

type AudioNodeVar = NamedVar AudioNode

data AudioNode
  = Oscillator OscillatorNode
  | Gain GainNode
  deriving (Eq, Show)

newtype GainNode = GainNode {gain :: AudioParam}
  deriving (Eq, Show)

data OscWaveType = Sine | Square | Sawtooth | Triangle | Custom
  deriving (Eq)

instance Show OscWaveType where
  show Sine = "sine"
  show Square = "square"
  show Sawtooth = "sawtooth"
  show Triangle = "triangle"
  show Custom = "custom"

data OscillatorNode = OscillatorNode
  { frequency :: AudioParam,
    detune :: AudioParam,
    waveType :: OscWaveType
  }
  deriving (Eq, Show)

createOscillatorNode ::
  AudioParamValue -> -- frequency value
  AudioParamValue -> -- detune value
  OscWaveType -> -- wave type
  AudioNode
createOscillatorNode frequencyValue detuneValue waveType =
  Oscillator
    OscillatorNode
      { frequency = AudioParam (FrequencyParam, frequencyValue),
        detune = AudioParam (DetuneParam, detuneValue),
        waveType = waveType
      }

updateAudioParamInAudioNodeVar :: AudioNodeVar -> AudioParam -> AudioNodeVar
updateAudioParamInAudioNodeVar (NamedVar varName node) newParam = NamedVar varName $ updateAudioParamInNode node newParam

-- TODO: Implement for all params in all nodes
-- TODO: Maybe this should be a method of an AudioNode class?
updateAudioParamInNode :: AudioNode -> AudioParam -> AudioNode
updateAudioParamInNode (Oscillator oscNode) newParam@(AudioParam (FrequencyParam, _)) = Oscillator oscNode {frequency = newParam}
updateAudioParamInNode (Oscillator oscNode) newParam@(AudioParam (DetuneParam, _)) = Oscillator oscNode {detune = newParam}
updateAudioParamInNode (Gain gainNode) newParam@(AudioParam (GainParam, _)) = Gain gainNode {gain = newParam}
updateAudioParamInNode _ _ = error "Invalid audio parameter for given node"

-- TODO: Implement for all params in all nodes
extractParamFromAudioNodeVar :: AudioNodeVar -> AudioParamType -> AudioParam
extractParamFromAudioNodeVar (NamedVar _ (Oscillator oscNode)) FrequencyParam = frequency oscNode
extractParamFromAudioNodeVar (NamedVar _ (Oscillator oscNode)) DetuneParam = detune oscNode
extractParamFromAudioNodeVar (NamedVar _ (Gain gainNode)) GainParam = gain gainNode
extractParamFromAudioNodeVar _ _ = error "Invalid audio parameter for given node"

-- TODO: Implement for all nodes
getParamsFromAudioNode :: AudioNode -> [AudioParam]
getParamsFromAudioNode (Oscillator oscNode) = [frequency oscNode, detune oscNode]
getParamsFromAudioNode (Gain gainNode) = [gain gainNode]