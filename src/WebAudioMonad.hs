{-# LANGUAGE FlexibleInstances #-}

module WebAudioMonad where

import AudioNode
import AudioParam
import Var

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

  -- TODO: HACER MAS GENERICA, O ASEGURARSE QUE EL GRAFO TMB SEA [Char]
  execute :: String -> m a -> IO FilePath
