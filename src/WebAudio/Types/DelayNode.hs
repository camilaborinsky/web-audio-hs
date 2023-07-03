module WebAudio.Types.DelayNode where

import WebAudio.Types.AudioParam

newtype DelayNode = DelayNode {delayDelayTime :: AudioParam}
  deriving (Eq, Show)

updateAudioParamInDelayNode :: DelayNode -> AudioParam -> DelayNode
updateAudioParamInDelayNode delayNode newParam@(AudioParam (DelayTimeParam, _)) = delayNode {delayDelayTime = newParam}
updateAudioParamInDelayNode _ _ = error "Invalid audio parameter for given DelayNode"

getParamsFromDelayNode :: DelayNode -> [AudioParam]
getParamsFromDelayNode delayNode = [delayDelayTime delayNode]
