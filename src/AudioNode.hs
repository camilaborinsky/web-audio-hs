module AudioNode where

import Var (NamedVar (..))

data AudioContext = AudioContext deriving (Show, Eq)

data AudioNode
  = OscillatorNode (NamedVar AudioContext)
  | GainNode (NamedVar AudioContext) -- TODO: testear
  | DelayNode (NamedVar AudioContext) -- TODO: testear
  | BiquadFilterNode (NamedVar AudioContext) -- TODO: testear
  | ConvolverNode (NamedVar AudioContext) -- TODO: testear
  | AnalyserNode (NamedVar AudioContext) -- TODO: testear
  | ChannelSplitterNode (NamedVar AudioContext) -- TODO: testear
  | ChannelMergerNode (NamedVar AudioContext) -- TODO: testear
  | DynamicsCompressorNode (NamedVar AudioContext) -- TODO: testear
  | WaveShaperNode (NamedVar AudioContext) -- TODO: testear
  | PannerNode (NamedVar AudioContext) -- TODO: testear
  | StereoPannerNode (NamedVar AudioContext) -- TODO: testear
  | IIRFilterNode (NamedVar AudioContext) -- TODO: testear
  | MediaElementAudioSourceNode (NamedVar AudioContext) -- TODO: testear
  | MediaStreamAudioSourceNode (NamedVar AudioContext) -- TODO: testear
  | MediaStreamAudioDestinationNode (NamedVar AudioContext) -- TODO: testear
  | ConstantSourceNode (NamedVar AudioContext) -- TODO: testear
  | AudioScheduledSourceNode (NamedVar AudioContext) -- TODO: testear
  | AudioDestinationNode (NamedVar AudioContext) -- TODO: testear
  | AudioBufferSourceNode (NamedVar AudioContext) -- TODO: testear
  deriving (Eq)

instance Show AudioNode where
  show (OscillatorNode _) = "OscillatorNode"
  show (GainNode _) = "GainNode"
  show (DelayNode _) = "DelayNode"
  show (BiquadFilterNode _) = "BiquadFilterNode"
  show (ConvolverNode _) = "ConvolverNode"
  show (AnalyserNode _) = "AnalyserNode"
  show (ChannelSplitterNode _) = "ChannelSplitterNode"
  show (ChannelMergerNode _) = "ChannelMergerNode"
  show (DynamicsCompressorNode _) = "DynamicsCompressorNode"
  show (WaveShaperNode _) = "WaveShaperNode"
  show (PannerNode _) = "PannerNode"
  show (StereoPannerNode _) = "StereoPannerNode"
  show (IIRFilterNode _) = "IIRFilterNode"
  show (MediaElementAudioSourceNode _) = "MediaElementAudioSourceNode"
  show (MediaStreamAudioSourceNode _) = "MediaStreamAudioSourceNode"
  show (MediaStreamAudioDestinationNode _) = "MediaStreamAudioDestinationNode"
  show (ConstantSourceNode _) = "ConstantSourceNode"
  show (AudioScheduledSourceNode _) = "AudioScheduledSourceNode"
  show (AudioDestinationNode _) = "AudioDestinationNode"
  show (AudioBufferSourceNode _) = "AudioBufferSourceNode"



type AudioNodeVar = NamedVar AudioNode

-- instance Eq AudioNodeVar where
--   (AudioNodeVar (_, varName1)) == (AudioNodeVar (_, varName2)) = varName1 == varName2

-- instance Ord AudioNodeVar where
--   compare (AudioNodeVar (_, varName1)) (AudioNodeVar (_, varName2)) = compare varName1 varName2