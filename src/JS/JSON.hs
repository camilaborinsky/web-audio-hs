{-# LANGUAGE OverloadedStrings #-}

module JS.JSON where

import Data.Aeson
import WebAudio.Types

instance ToJSON AudioContext where
  toJSON AudioContext = object []

instance ToJSON AudioNode where
  toJSON (Oscillator node) = toJSON node
  toJSON (Gain node) = toJSON node

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