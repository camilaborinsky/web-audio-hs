module WebAudio.Types.AudioNode where

import Var (NamedVar (..))
import WebAudio.Types.AudioParam
import WebAudio.Types.BiquadFilterNode
import WebAudio.Types.DelayNode
import WebAudio.Types.GainNode
import WebAudio.Types.OscillatorNode

type AudioNodeVar = NamedVar AudioNode

data AudioNode
  = Oscillator OscillatorNode
  | Gain GainNode
  | BiquadFilter BiquadFilterNode
  | Delay DelayNode
  | Destination
  deriving (Eq, Show)

createOscillatorNode ::
  AudioParamValue -> -- frequency value
  AudioParamValue -> -- detune value
  OscWaveType -> -- wave type
  AudioNode
createOscillatorNode frequencyValue detuneValue waveType =
  Oscillator
    OscillatorNode
      { oscFrequency = AudioParam (FrequencyParam, frequencyValue),
        oscDetune = AudioParam (DetuneParam, detuneValue),
        oscWaveType = waveType
      }

createGainNode :: AudioParamValue -> AudioNode
createGainNode gainValue = Gain GainNode {gainGain = AudioParam (GainParam, gainValue)}

createBiquadFilterNode ::
  BiquadFilterType -> -- filter type
  AudioParamValue -> -- frequency value
  AudioParamValue -> -- detune value
  AudioParamValue -> -- Q value
  AudioParamValue -> -- gain value
  AudioNode
createBiquadFilterNode filterType frequencyValue detuneValue qValue gainValue =
  BiquadFilter
    BiquadFilterNode
      { biquadFilterType = filterType,
        biquadFilterFrequency = AudioParam (FrequencyParam, frequencyValue),
        biquadFilterDetune = AudioParam (DetuneParam, detuneValue),
        biquadFilterQ = AudioParam (QParam, qValue),
        biquadFilterGain = AudioParam (GainParam, gainValue)
      }

createDelayNode :: AudioParamValue -> AudioNode
createDelayNode delayTimeValue =
  Delay
    DelayNode
      { delayDelayTime = AudioParam (DelayTimeParam, delayTimeValue)
      }

updateAudioParamInAudioNodeVar :: AudioNodeVar -> AudioParam -> AudioNodeVar
updateAudioParamInAudioNodeVar (NamedVar varName node) newParam = NamedVar varName $ updateAudioParamInNode node newParam

updateAudioParamInNode :: AudioNode -> AudioParam -> AudioNode
updateAudioParamInNode (Oscillator oscNode) param = Oscillator $ updateAudioParamInOscillatorNode oscNode param
updateAudioParamInNode (Gain gainNode) param = Gain $ updateAudioParamInGainNode gainNode param
updateAudioParamInNode (Delay delayNode) param = Delay $ updateAudioParamInDelayNode delayNode param
updateAudioParamInNode (BiquadFilter biquadNode) param = BiquadFilter $ updateAudioParamInBiquadFilterNode biquadNode param
updateAudioParamInNode _ _ = error "Invalid audio parameter for given node"

extractParamFromAudioNode :: AudioNode -> AudioParamType -> AudioParam
extractParamFromAudioNode (Oscillator oscNode) = extractParamFromOscillatorNode oscNode
extractParamFromAudioNode (Gain gainNode) = extractParamFromGainNode gainNode
extractParamFromAudioNode (Delay delayNode) = extractParamFromDelayNode delayNode
extractParamFromAudioNode (BiquadFilter biquadNode) = extractAudioParamFromBiquadFilterNode biquadNode
extractParamFromAudioNode _  = error "Invalid audio parameter for given node"

getParamsFromAudioNode :: AudioNode -> [AudioParam]
getParamsFromAudioNode (Oscillator oscNode) = getParamsFromOscillatorNode oscNode
getParamsFromAudioNode (Gain gainNode) = getParamsFromGainNode gainNode
getParamsFromAudioNode (Delay delayNode) = getParamsFromDelayNode delayNode
getParamsFromAudioNode (BiquadFilter biquadNode) =
 getParamsFromBiquadFilterNode biquadNode
getParamsFromAudioNode Destination = []

getAudioNodeType :: AudioNode -> String
getAudioNodeType (Oscillator _) = "OscillatorNode"
getAudioNodeType (Gain _) = "GainNode"
getAudioNodeType (Delay _) = "DelayNode"
getAudioNodeType (BiquadFilter _) = "BiquadFilterNode"
getAudioNodeType Destination = "AudioDestinationNode"