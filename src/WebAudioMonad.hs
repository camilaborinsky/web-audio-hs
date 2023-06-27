-- {-# LANGUAGE TypeSynonymInstances #-}

module WebAudioMonad where

import AudioNode
import Var

data AudioContext = AudioContext

instance Ord AudioNodeVar where
  compare (NamedVar varName1 _) (NamedVar varName2 _) = compare varName1 varName2

class Monad m => WebAudioMonad m where
  -- Add functions specific to WebAudioApi here

  -- createContext :: [Char] -> m AudioContext

  -- Create an audio node
  createNode :: AudioNodeVar -> m AudioNodeVar

  -- Connect output of first node to the input of the second node
  connect :: AudioNodeVar -> AudioNodeVar -> m ()

  -- Disconnect output of first node from the input of the second node
  disconnect :: AudioNodeVar -> AudioNodeVar -> m ()

  -- TODO: HACER MAS GENERICA, O ASEGURARSE QUE EL GRAFO TMB SEA [Char]
  execute :: m a -> IO FilePath
