{-# LANGUAGE OverloadedStrings #-}

module JS.Encode where

import AudioNode
import AudioParam
import Data.Aeson

instance ToJSON OscillatorNode where
  toJSON (OscillatorNode (AudioParam (FrequencyParam, frequencyValue)) (AudioParam (DetuneParam, detuneValue)) waveType) =
    object
      [ "frequency" .= frequencyValue,
        "detune" .= detuneValue,
        "type" .= show waveType -- this will map to  "type" in JSON
      ]

instance ToJSON GainNode where
  toJSON (GainNode (AudioParam (GainParam, gainValue))) =
    object
      [ "gain" .= gainValue
      ]