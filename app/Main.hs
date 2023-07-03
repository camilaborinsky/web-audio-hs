import GraphDrawer.WebAudioApiGraphDrawer
import JS.WebAudioApiJS
import Var
import WebAudio.Types
import WebAudio.WebAudioMonad

exampleUsage :: WebAudioApiGraphDrawer ()
exampleUsage = do
  let oscNode = createOscillatorNode 0.5 1.5 Sine
  let gainNode = createGainNode 0.7
  node1 <- createNode oscNode "oscillator"
  node2 <- createNode gainNode "gain"

  connect node1 node2
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