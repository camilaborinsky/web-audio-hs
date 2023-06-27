module AudioNode where

import Var (NamedVar (..))

data AudioContext = AudioContext deriving (Show, Eq)

data AudioNode
  = OscillatorNode (NamedVar AudioContext)
  | GainNode Double (NamedVar AudioContext)
  | DelayNode Double (NamedVar AudioContext)
  deriving (Show, Eq)

type AudioNodeVar = NamedVar AudioNode

-- instance Eq AudioNodeVar where
--   (AudioNodeVar (_, varName1)) == (AudioNodeVar (_, varName2)) = varName1 == varName2

-- instance Ord AudioNodeVar where
--   compare (AudioNodeVar (_, varName1)) (AudioNodeVar (_, varName2)) = compare varName1 varName2