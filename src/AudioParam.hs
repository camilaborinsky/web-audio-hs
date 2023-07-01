module AudioParam where
import AudioNode
import Var (NamedVar)
type AudioParamValue = Double

-- -- data AudioParam = Gain AudioParamValue 
-- --                 | DelayTime AudioParamValue 
-- --                 | Frequency AudioParamValue 
-- --                 | Q AudioParamValue 
-- --                 | Detune AudioParamValue 
-- --                 | Pan AudioParamValue 
-- --   deriving (Show, Eq)

-- data GainParam = Gain | GainValue AudioParamValue


data AudioParamType = Gain | DelayTime | Frequency | Q | Detune | Pan
  deriving (Show, Eq)

newtype AudioParam = (AudioParamType, AudioParamValue)
  deriving (Show, Eq)

setParam :: AudioNodeVar -> AudioParam -> AudioParamValue -> m AudioNodeVar
setParam (NamedVar name (GainNode context)) Gain value = do 
    tell $ "setParam " ++ name ++ " Gain " ++ show value
    return $ 


getParam :: AudioNodeVar -> AudioParam -> m (AudioNodeVar, AudioParam)



connectToParam :: AudioNodeVar -> AudioNodeVar -> AudioParam -> m ()





getAudioParam :: AudioNodeVar -> GainParam -> m AudioParamValue
getAudioParam n Gain =  


-- applyGain :: AudioNodeVar -> AudioParamValue -> m AudioNodeVar
-- applyGain (NamedVar name (GainNode context)) = -- string para setear

-- connectToGain :: AudioNodeVar -> AudioNodeVar -> m ()
-- connectToGain n1 (NamedVar name (GainNode context)) = --string
