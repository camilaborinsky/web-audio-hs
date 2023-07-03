import AudioNode
import AudioParam
import Var
import WebAudioApiGraphDrawer
import WebAudioApiJS
import WebAudioMonad

exampleUsage :: WebAudioApiGraphDrawer ()
exampleUsage = do
  let oscNode1 = createOscillatorNode 0.5 1.5 Sine
  let oscNode2 = createOscillatorNode 0.5 0.1 Sine
  node1 <- createNode oscNode1 "oscillator1"
  node2 <- createNode oscNode2 "oscillator2"

  setAudioParam node1 $ AudioParam (FrequencyParam, 440)
  detune <- getAudioParam node1 DetuneParam "detune"
  setAudioParam node2 (varValue detune)
  param <- getAudioParam node1 FrequencyParam "frequency"
  connectToParam node2 node1 param
  disconnectFromParam node2 node1 param
  connect node1 node2
  return ()

main :: IO FilePath
main = do
  -- let (node, _) = runWebAudioApiCompiler exampleUsage
  execute "./tmp/prueba" exampleUsage

-- putStrLn $ executeString exampleUsage
