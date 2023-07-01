import AudioNode
import AudioParam
import Var
import WebAudioApiGraphDrawer
import WebAudioApiJS
import WebAudioMonad

exampleUsage :: WebAudioApiGraphDrawer ()
exampleUsage = do
  let oscNode1 = createOscillatorNode 0.5 0.1 Sine
  let oscNode2 = createOscillatorNode 0.5 0.1 Sine
  node1 <- createNode oscNode1 "oscillator1"
  node2 <- createNode oscNode2 "oscillator2"

  -- gainNode.gain
  -- const gainParam = gainNode.gain

  -- connectParam node1 node2 (Gain 0.2)
  connect node1 node2
  -- disconnect node1 node2
  return ()

-- const gainParam = gainNode . gain

-- lfo . connect (gainParam)

-- lfo . connect (gainNode . gain)

main :: IO FilePath
main = do
  -- let (node, _) = runWebAudioApiCompiler exampleUsage
  execute exampleUsage

-- putStrLn $ executeString exampleUsage
