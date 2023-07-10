{-# LANGUAGE FlexibleInstances #-}

module WebAudio.WebAudioMonad where

import Var
import WebAudio.Types

instance Ord AudioNodeVar where
  compare (NamedVar varName1 _) (NamedVar varName2 _) = compare varName1 varName2

class Monad m => WebAudioMonad m where
  -- Create an audio node in the context given the node and a variable name
  createNode :: AudioNode -> String -> m AudioNodeVar

  -- Set AudioParam to an AudioNode
  setAudioParam :: AudioNodeVar -> AudioParam -> m AudioNodeVar

  -- Get AudioParam from an AudioNode, assing it to a variable
  getAudioParam :: AudioNodeVar -> AudioParamType -> String -> m AudioParamVar

  -- Connect output of first node to the input of the second node
  connect :: AudioNodeVar -> AudioNodeVar -> m ()

  -- Disconnect output of first node from the input of the second node
  disconnect :: AudioNodeVar -> AudioNodeVar -> m ()

  -- Connect output of first node to another audionode's audioparam
  connectToParam :: AudioNodeVar -> AudioNodeVar -> AudioParamVar -> m ()

  -- Disconnect output of first node from another audionode's audioparam
  disconnectFromParam :: AudioNodeVar -> AudioNodeVar -> AudioParamVar -> m ()

  -- Start the oscillator node
  startOscillatorNode :: AudioNodeVar -> m ()
  startOscillatorNode = Prelude.flip startOscillatorNodeAtTime 0

  -- Stop the oscillator node
  stopOscillatorNode :: AudioNodeVar -> m ()
  stopOscillatorNode = Prelude.flip stopOscillatorNodeAtTime 0

  startOscillatorNodeAtTime :: AudioNodeVar -> Double -> m ()

  stopOscillatorNodeAtTime :: AudioNodeVar -> Double -> m ()

  connectToDestination :: AudioNodeVar -> m ()

  -- Set the value of an AudioParam at a given time 
  -- (https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueAtTime)
  setValueAtTime ::
    AudioParamVar ->
    Double -> -- value
    Double -> -- time
    m ()

  -- Set the value of an AudioParam linearly interpolated at a given time 
  -- (https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/linearRampToValueAtTime)
  linearRampToValueAtTime ::
    AudioParamVar ->
    Double -> -- value
    Double -> -- time
    m ()

  -- Set the value of an AudioParam exponentially interpolated at a given time 
  -- (https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/exponentialRampToValueAtTime)
  exponentialRampToValueAtTime ::
    AudioParamVar ->
    Double -> -- value
    Double -> -- time
    m ()

  -- schedules the start of a gradual change to the AudioParam value 
  -- (https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setTargetAtTime)
  setTargetAtTime ::
    AudioParamVar ->
    Double -> -- target
    Double -> -- time
    Double -> -- timeConstant
    m ()

  -- schedules the parameter's value to change following a curve defined by a list of values.
  -- (https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime)
  setValueCurveAtTime ::
    AudioParamVar ->
    [Double] -> -- values
    Double -> -- time
    Double -> -- duration
    m ()

  --  cancels all scheduled future changes to the AudioParam. 
  -- (https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/cancelScheduledValues)
  cancelScheduledValues ::
    AudioParamVar ->
    Double -> -- startTime
    m ()

  -- cancels all scheduled future changes to the AudioParam but holds its value at a given time until further changes are made using other methods.
  -- (https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime)
  cancelAndHoldAtTime ::
    AudioParamVar ->
    Double -> -- startTime
    m ()

  execute :: String -> m a -> IO FilePath
