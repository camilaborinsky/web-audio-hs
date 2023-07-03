{-# LANGUAGE OverloadedStrings #-}

module WebAudio.AudioGraph.AudioNode.DelayNode where

import Data.Aeson (ToJSON (..), object, (.=))
import WebAudio.AudioGraph.AudioNode.Types
import WebAudio.AudioGraph.AudioParam

newtype DelayNode = DelayNode {delayTime :: AudioParam} deriving (Eq, Show)

instance AudioNodeBehavior DelayNode where
  updateAudioParamInNode delayNode newParam@(AudioParam (DelayTimeParam, _)) = delayNode {delayTime = newParam}
  updateAudioParamInNode _ _ = error "Invalid audio parameter for Delay node"

  extractParamFromAudioNode delayNode DelayTimeParam = delayTime delayNode
  extractParamFromAudioNode _ _ = error "Invalid audio parameter for Delay node"

  getParamsFromAudioNode delayNode = [delayTime delayNode]

  getAudioNodeType _ = "DelayNode"

instance ToJSON DelayNode where
  toJSON (DelayNode (AudioParam (DelayTimeParam, delayTimeValue))) =
    object
      [ "delayTime" .= delayTimeValue
      ]

createDelayNode :: AudioParamValue -> AudioNode
createDelayNode delayTimeParam = AudioNode $ DelayNode $ AudioParam (DelayTimeParam, delayTimeParam)