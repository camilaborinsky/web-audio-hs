import WebAudioApiCompiler
import WebAudioMonad

exampleUsage :: WebAudioApiCompiler ()
exampleUsage = do
  node <- createNode OscillatorNode "oscillator1"
  node <- createNode OscillatorNode "oscillator2"
  return ()

main :: IO ()
main = do
  -- let (node, _) = runWebAudioApiCompiler exampleUsage
  putStrLn $ execute exampleUsage



