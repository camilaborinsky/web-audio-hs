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

-- data GainNode = GainNode (NamedVar AudioContext) -- TODO: testear
-- data DelayNode = DelayNode (NamedVar AudioContext) -- TODO: testear
-- data BiquadFilterNode = BiquadFilterNode (NamedVar AudioContext) -- TODO: testear
-- data ConvolverNode = ConvolverNode (NamedVar AudioContext) -- TODO: testear
-- data AnalyserNode = AnalyserNode (NamedVar AudioContext) -- TODO: testear
-- data ChannelSplitterNode = ChannelSplitterNode (NamedVar AudioContext) -- TODO: testear
-- data ChannelMergerNode = ChannelMergerNode (NamedVar AudioContext) -- TODO: testear
-- data DynamicsCompressorNode = DynamicsCompressorNode (NamedVar AudioContext) -- TODO: testear
-- data WaveShaperNode = WaveShaperNode (NamedVar AudioContext) -- TODO: testear
-- data PannerNode = PannerNode (NamedVar AudioContext) -- TODO: testear
-- data StereoPannerNode = StereoPannerNode (NamedVar AudioContext) -- TODO: testear
-- data IIRFilterNode = IIRFilterNode (NamedVar AudioContext) -- TODO: testear
-- data MediaElementAudioSourceNode = MediaElementAudioSourceNode (NamedVar AudioContext) -- TODO: testear
-- data MediaStreamAudioSourceNode = MediaStreamAudioSourceNode (NamedVar AudioContext) -- TODO: testear
-- data MediaStreamAudioDestinationNode = MediaStreamAudioDestinationNode (NamedVar AudioContext) -- TODO: testear
-- data ConstantSourceNode = ConstantSourceNode (NamedVar AudioContext) -- TODO: testear
-- data AudioScheduledSourceNode = AudioScheduledSourceNode (NamedVar AudioContext) -- TODO: testear
-- data AudioDestinationNode = AudioDestinationNode (NamedVar AudioContext) -- TODO: testear
-- data AudioBufferSourceNode = AudioBufferSourceNode (NamedVar AudioContext) -- TODO: testear

-- instance Show AudioNode where
--   show (OscillatorNode _) = "OscillatorNode"
--   show (GainNode _) = "GainNode"
--   show (DelayNode _) = "DelayNode"
--   show (BiquadFilterNode _) = "BiquadFilterNode"
--   show (ConvolverNode _) = "ConvolverNode"
--   show (AnalyserNode _) = "AnalyserNode"
--   show (ChannelSplitterNode _) = "ChannelSplitterNode"
--   show (ChannelMergerNode _) = "ChannelMergerNode"
--   show (DynamicsCompressorNode _) = "DynamicsCompressorNode"
--   show (WaveShaperNode _) = "WaveShaperNode"
--   show (PannerNode _) = "PannerNode"
--   show (StereoPannerNode _) = "StereoPannerNode"
--   show (IIRFilterNode _) = "IIRFilterNode"
--   show (MediaElementAudioSourceNode _) = "MediaElementAudioSourceNode"
--   show (MediaStreamAudioSourceNode _) = "MediaStreamAudioSourceNode"
--   show (MediaStreamAudioDestinationNode _) = "MediaStreamAudioDestinationNode"
--   show (ConstantSourceNode _) = "ConstantSourceNode"
--   show (AudioScheduledSourceNode _) = "AudioScheduledSourceNode"
--   show (AudioDestinationNode _) = "AudioDestinationNode"
--   show (AudioBufferSourceNode _) = "AudioBufferSourceNode"

-- instance Eq AudioNodeVar where
--   (AudioNodeVar (_, varName1)) == (AudioNodeVar (_, varName2)) = varName1 == varName2

-- instance Ord AudioNodeVar where
--   compare (AudioNodeVar (_, varName1)) (AudioNodeVar (_, varName2)) = compare varName1 varName2

-- setParam :: AudioNode -> AudioParam -> AudioParamValue -> m AudioNode
-- setParam (OscillatorNode context gain frequency) Gain value = do
--     newNode <- OscillatorNode context value frequency
--     return newNode
-- setParam (OscillatorNode context gain frequency) Frequency value = do
--     newNode <- OscillatorNode context gain value
--     return newNode

-- En el main
-- createNode :: AudioNode -> NamedVar AudioContext -> m AudioNodeVar
-- gainNode <- createNode GainDefault "gain1" context
-- gainComplete <- createNode Gain (GainNode 20) "gain2" context
-- setAudioParam gainComplete Gain 40