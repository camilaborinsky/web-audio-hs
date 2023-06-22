import WebAudioMonad

exampleUsage :: WebAudioApiCompiler ()
exampleUsage = do
  node <- createNode OscillatorNode "oscillator"
  execute node
