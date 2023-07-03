{-# LANGUAGE GADTs #-}

module WebAudio.AudioGraph.AudioNode.Types where

import Data.Aeson
import Var (NamedVar (..))
import WebAudio.AudioGraph.AudioParam

-- data AudioNodeVar where
--   AudioNodeVar :: AudioNodeBehavior a => NamedVar a -> AudioNodeVar



data AudioNode where
  AudioNode :: (AudioNodeBehavior a, ToJSON a) => a -> AudioNode

type AudioNodeVar = NamedVar AudioNode

instance Eq AudioNode where
  (AudioNode node1) == (AudioNode node2) = getAudioNodeType node1 == getAudioNodeType node2 && getParamsFromAudioNode node1 == getParamsFromAudioNode node2


class AudioNodeBehavior a where
  updateAudioParamInNode :: a -> AudioParam -> a
  extractParamFromAudioNode :: a -> AudioParamType -> AudioParam
  getParamsFromAudioNode :: a -> [AudioParam]
  getAudioNodeType :: a -> String

instance ToJSON AudioNode where
  toJSON (AudioNode node) = toJSON node

-- data AudioNode a
--   = Oscillator OscillatorNode
--   | Gain GainNode
--   | Delay DelayNode
--   | BiquadFilter BiquadFilterNode
--   deriving (Eq, Show)

-- createOscillatorNode ::
--   AudioParamValue -> -- frequency value
--   AudioParamValue -> -- detune value
--   OscWaveType -> -- wave type
--   AudioNode
-- createOscillatorNode = Oscillator osc.createOscillatorNode

-- updateAudioParamInAudioNodeVar :: AudioNodeVar -> AudioParam -> AudioNodeVar
-- updateAudioParamInAudioNodeVar (NamedVar varName node) newParam = NamedVar varName $ updateAudioParamInNode node newParam

-- updateAudioParamInNode :: AudioNode -> AudioParam -> AudioNode
-- updateAudioParamInNode (Oscillator oscNode) newParam = updateAudioParamInOscillatorNode oscNode newParam
-- updateAudioParamInNode (Gain gainNode) newParam = updateAudioParamInGainNode gainNode newParam
-- updateAudioParamInNode (DelayNode delayNode) newParam = updateAudioParamInDelayNode delayNode newParam
-- updateAudioParamInNode (BiquadFilterNode biquadFilterNode) newParam = updateAudioParamInBiquadFilterNode biquadFilterNode newParam
-- updateAudioParamInNode _ _ = error "Invalid audio node type"

-- extractParamFromAudioNodeVar :: AudioNodeVar -> AudioParamType -> AudioParam
-- extractParamFromAudioNodeVar (NamedVar _ (Oscillator oscNode)) = extractParamFromOscillatorNode oscNode
-- extractParamFromAudioNodeVar (NamedVar _ (Gain gainNode)) = extractParamFromGainNode gainNode
-- extractParamFromAudioNodeVar (NamedVar _ (Delay delayNode)) = extractParamFromDelayNode delayNode
-- extractParamFromAudioNodeVar (NamedVar _ (BiquadFilter biquadFilterNode)) = extractParamFromBiQuadFilterNode biquadFilterNode
-- extractParamFromAudioNodeVar _ _ = error "Invalid audio node"

-- getParamsFromAudioNode :: AudioNode -> [AudioParam]
-- getParamsFromAudioNode (Oscillator oscNode) = [frequency oscNode, detune oscNode]
-- getParamsFromAudioNode (Gain gainNode) = [gain gainNode]
-- getParamsFromAudioNode (Delay delayNode) = [delayTime delayNode]
-- getParamsFromAudioNode (BiquadFilter biquadFilterNode) = [frequency biquadFilterNode, detune biquadFilterNode, qParam biquadFilterNode, gain biquadFilterNode]

-- getAudioNodeType :: AudioNode -> String
-- getAudioNodeType (Oscillator _) = "OscillatorNode"
-- getAudioNodeType (Gain _) = "GainNode"
-- getAudioNodeType (Delay _) = "DelayNode"
-- getAudioNodeType (BiquadFilter _) = "BiquadFilterNode"