import WebAudioApiCompiler
import WebAudioApiGraphDrawer
import WebAudioMonad

exampleUsage :: WebAudioApiGraphDrawer ()
exampleUsage = do
  node <- createNode $ AudioNodeVar (OscillatorNode, "oscillator1")
  node <- createNode $ AudioNodeVar (OscillatorNode, "oscillator2")
  return ()

main :: IO ()
main = do
  -- let (node, _) = runWebAudioApiCompiler exampleUsage
  putStrLn $ execute exampleUsage



