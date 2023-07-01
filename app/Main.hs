import AudioNode
import Var
import WebAudioApiCompiler
import WebAudioApiGraphDrawer
import WebAudioMonad
import AudioParam

exampleUsage :: WebAudioApiCompiler ()
exampleUsage = do
  context <- createContext $ NamedVar {varValue = AudioContext, varName = "context"}
  node1 <- createNode $ NamedVar {varValue = OscillatorNode context, varName = "oscillator1"}
  node2 <- createNode $ NamedVar {varValue = OscillatorNode context, varName = "oscillator2"}
  lfo <- createNode $ NamedVar {varValue = OscillatorNode context, varName = "lfo"}

  -- gainNode.gain
  -- const gainParam = gainNode.gain


  connectParam node1 node2 (Gain 0.2)
  connect lfo gain
  connect node1 node2
  -- disconnect node1 node2
  return ()

const gainParam = gainNode.gain
lfo.connect(gainParam)


lfo.connect(gainNode.gain)

main :: IO FilePath
main = do
  -- let (node, _) = runWebAudioApiCompiler exampleUsage
  execute exampleUsage

-- putStrLn $ executeString exampleUsage
