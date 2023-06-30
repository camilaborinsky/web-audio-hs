module AudioParam where

import Var

type AudioParamValue = Double

data AudioParam
  = Gain AudioParamValue
  | DelayTime AudioParamValue
  | Frequency AudioParamValue
  | Q AudioParamValue
  | Detune AudioParamValue
  | Pan AudioParamValue
  deriving (Show, Eq)

type AudioParamVar = NamedVar AudioParam