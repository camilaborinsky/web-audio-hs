module WebAudio.Types.GainNode where

import WebAudio.Types.AudioParam

newtype GainNode = GainNode {gainGain :: AudioParam}
  deriving (Eq, Show)

updateAudioParamInGainNode :: GainNode -> AudioParam -> GainNode
updateAudioParamInGainNode gainNode newParam@(AudioParam (GainParam, _)) = gainNode {gainGain = newParam}
updateAudioParamInGainNode _ _ = error "Invalid audio parameter for given GainNode"

getParamsFromGainNode :: GainNode -> [AudioParam]
getParamsFromGainNode gainNode = [gainGain gainNode]