instance AudioNode OscillatorNode String OscillatorNodeAudioParams where
  compileInit :: NamedVar OscillatorNode -> String
  compileSetAudioParam :: NamedVar OscillatorNode -> OscillatorNodeAudioParams -> String
  compilesetAudioParam node param = 

instance AudioNode GainNode String GainNodeParams where
  compileSetAudioParam :: NamedVar GainNode -> GainNodeParams -> String
  


instance AudioNode GainNode String Q

data GainNodeAudioParams =  Gain deriving (Eq, Show)

data OscillatorNodeAudioParams = Detune | Frequency deriving (Eq, Show)

