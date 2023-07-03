module WebAudio.Types.BiquadFilterNode where

import WebAudio.Types.AudioParam

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
  { biquadFilterFrequency :: AudioParam,
    biquadFilterDetune :: AudioParam,
    biquadFilterQ :: AudioParam,
    biquadFilterGain :: AudioParam,
    biquadFilterType :: BiquadFilterType
  }
  deriving (Eq, Show)

updateAudioParamInBiquadFilterNode :: BiquadFilterNode -> AudioParam -> BiquadFilterNode
updateAudioParamInBiquadFilterNode biquadNode newParam@(AudioParam (FrequencyParam, _)) = biquadNode {biquadFilterFrequency = newParam}
updateAudioParamInBiquadFilterNode biquadNode newParam@(AudioParam (DetuneParam, _)) = biquadNode {biquadFilterDetune = newParam}
updateAudioParamInBiquadFilterNode biquadNode newParam@(AudioParam (QParam, _)) = biquadNode {biquadFilterQ = newParam}
updateAudioParamInBiquadFilterNode biquadNode newParam@(AudioParam (GainParam, _)) = biquadNode {biquadFilterGain = newParam}
updateAudioParamInBiquadFilterNode _ _ = error "Invalid audio parameter for given BiquadFilterNode"

getParamsFromBiquadFilterNode :: BiquadFilterNode -> [AudioParam]
getParamsFromBiquadFilterNode biquadFilterNode = [biquadFilterFrequency biquadFilterNode, biquadFilterDetune biquadFilterNode, biquadFilterQ biquadFilterNode, biquadFilterGain biquadFilterNode]
