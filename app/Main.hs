import AudioNode
import Var
import WebAudioApiCompiler
import WebAudioApiGraphDrawer
import WebAudioMonad

exampleUsage :: WebAudioApiGraphDrawer ()
exampleUsage = do
  context <- createContext $ NamedVar {varValue = AudioContext, varName = "context"}
  node1 <- createNode $ NamedVar {varValue = OscillatorNode context, varName = "oscillator1"}
  node2 <- createNode $ NamedVar {varValue = OscillatorNode context, varName = "oscillator2"}
  connect node1 node2
  -- disconnect node1 node2
  return ()

main :: IO FilePath
main = do
  -- let (node, _) = runWebAudioApiCompiler exampleUsage
  execute exampleUsage

-- putStrLn $ executeString exampleUsage
