{-# LANGUAGE OverloadedStrings #-}

module WebAudio.AudioGraph.AudioNode.GainNode where
import Data.Aeson (ToJSON (..), object, (.=))
import WebAudio.AudioGraph.AudioNode.Types
import WebAudio.AudioGraph.AudioParam

newtype GainNode = GainNode {gain :: AudioParam}
  deriving (Eq, Show)

instance AudioNodeBehavior GainNode where
  updateAudioParamInNode gainNode newParam@(AudioParam (GainParam, _)) = gainNode {gain = newParam}
  updateAudioParamInNode _ _ = error "Invalid audio parameter for Gain node"

  extractParamFromAudioNode gainNode GainParam = gain gainNode
  extractParamFromAudioNode _ _ = error "Invalid audio parameter for Gain node"

  getParamsFromAudioNode gainNode = [gain gainNode]

  getAudioNodeType _ = "GainNode"

createGainNode :: AudioParamValue -> AudioNode
createGainNode gainValue = AudioNode GainNode {gain = AudioParam (GainParam, gainValue)}

instance ToJSON GainNode where
  toJSON (GainNode (AudioParam (GainParam, gainValue))) =
    object
      [ "gain" .= gainValue
      ]




