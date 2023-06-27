import WebAudioApiCompiler
import WebAudioApiGraphDrawer
import WebAudioMonad

exampleUsage :: WebAudioApiCompiler ()
exampleUsage = do
  node <- createNode $ AudioNodeVar (OscillatorNode, "oscillator1")
  node <- createNode $ AudioNodeVar (OscillatorNode, "oscillator2")
  return ()

main :: IO FilePath
main = do
  -- let (node, _) = runWebAudioApiCompiler exampleUsage
  putStrLn $ executeString exampleUsage
  execute exampleUsage
