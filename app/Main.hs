import WebAudioMonad

exampleUsage :: WebAudioApiCompiler ()
exampleUsage = do
  node <- createNode OscillatorNode "oscillator"
  return ()

main :: IO ()
main = do
  -- let (node, _) = runWebAudioApiCompiler exampleUsage
  putStrLn $ execute exampleUsage



