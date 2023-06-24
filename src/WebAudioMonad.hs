-- {-# LANGUAGE TypeSynonymInstances #-}

module WebAudioMonad where


data AudioNode
  = OscillatorNode
  | GainNode Double
  | DelayNode Double


class Monad m => WebAudioMonad m where
  -- Add functions specific to WebAudioApi here

  -- createContext :: [Char] -> m AudioContext

  -- Create an audio node
  createNode :: AudioNode -> [Char] -> m AudioNode

  -- TODO: HACER MAS GENERICA, O ASEGURARSE QUE EL GRAFO TMB SEA [Char]
  execute :: m a -> [Char]
