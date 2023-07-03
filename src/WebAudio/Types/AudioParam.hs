module WebAudio.Types.AudioParam where

import Var (NamedVar)

type AudioParamValue = Double

data AudioParamType = GainParam | DelayTimeParam | FrequencyParam | QParam | DetuneParam | PanParam
  deriving (Eq)

instance Show AudioParamType where
  show GainParam = "gain"
  show DelayTimeParam = "delayTime"
  show FrequencyParam = "frequency"
  show QParam = "Q"
  show DetuneParam = "detune"
  show PanParam = "pan"

newtype AudioParam = AudioParam (AudioParamType, AudioParamValue)
  deriving (Eq)

instance Show AudioParam where
  show (AudioParam (paramType, _)) = show paramType

type AudioParamVar = NamedVar AudioParam
