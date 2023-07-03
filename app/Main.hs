import GraphDrawer.WebAudioApiGraphDrawer
import JS.WebAudioApiJS
import Var
import WebAudio.AudioGraph
import WebAudio.WebAudioMonad

exampleUsage :: WebAudioApiGraphDrawer ()
exampleUsage = do
  let oscNode = createOscillatorNode 0.5 1.5 Sine
  let gainNode = createGainNode 0.7
  let delayNode = createDelayNode 0.5
  node1 <- createNode oscNode "oscillator"
  node2 <- createNode gainNode "gain"
  node3 <- createNode delayNode "delay"

  connect node1 node2
  connectToParam node2 node3 (getAudioParam node3 DelayTimeParam "delayTime")
  return ()

main :: IO FilePath
main = do
  execute "./output/out" exampleUsage

-- let (node, _) = runWebAudioApiCompiler exampleUsage
-- putStrLn $ executeString exampleUsage

-- setAudioParam node1 $ AudioParam (FrequencyParam, 440)
-- detune <- getAudioParam node1 DetuneParam "detune"
-- setAudioParam node2 (varValue detune)
-- param <- getAudioParam node1 FrequencyParam "frequency"
-- connectToParam node2 node1 param
-- disconnectFromParam node2 node1 param