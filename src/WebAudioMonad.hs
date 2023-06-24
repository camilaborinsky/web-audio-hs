-- {-# LANGUAGE TypeSynonymInstances #-}

module WebAudioMonad where

data AudioContext = AudioContext

data AudioNode
  = OscillatorNode
  | GainNode Double
  | DelayNode Double

newtype AudioNodeVar = AudioNodeVar (AudioNode, [Char])

instance Eq AudioNodeVar where
  (AudioNodeVar (_, varName1)) == (AudioNodeVar (_, varName2)) = varName1 == varName2

instance Ord AudioNodeVar where 
  compare (AudioNodeVar (_, varName1)) (AudioNodeVar (_, varName2)) = compare varName1 varName2

class Monad m => WebAudioMonad m where
  -- Add functions specific to WebAudioApi here

  -- createContext :: [Char] -> m AudioContext

  -- Create an audio node
  createNode :: AudioNodeVar -> m AudioNodeVar

  -- TODO: HACER MAS GENERICA, O ASEGURARSE QUE EL GRAFO TMB SEA [Char]
  execute :: m a  -> IO FilePath
