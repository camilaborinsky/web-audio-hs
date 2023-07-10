import GraphDrawer.WebAudioApiGraphDrawer
import JS.WebAudioApiJS
import Var
import WebAudio.Types
import WebAudio.WebAudioMonad

---- EJEMPLO 1: Crear oscilador de onda cuadrada de 440 Hz ----

-- En JavaScript:
-- const audioCtx = new (window.AudioContext || window.webkitAudioContext)();

-- const oscillator = audioCtx.createOscillator();

-- oscillator.type = "square";
-- oscillator.frequency.setValueAtTime(440, audioCtx.currentTime); // value in hertz
-- oscillator.connect(audioCtx.destination);
-- oscillator.start();

oscillatorEx1 :: WebAudioApiGraphDrawer ()
oscillatorEx1 = do
  let oscNode = createOscillatorNode 440 0 Square
  node <- createNode oscNode "oscillator"
  connectToDestination node
  return ()



---- EJEMPLO 2: Conectar dos nodos ----

-- En JS:
-- const audioCtx = new AudioContext();

-- const oscillator = audioCtx.createOscillator();
-- const gainNode = audioCtx.createGain();

-- oscillator.connect(gainNode);
-- gainNode.connect(audioCtx.destination);


connectExample :: WebAudioApiGraphDrawer ()
connectExample = do
  let oscNode = createOscillatorNode 440 0 Square
  let gainNode = createGainNode 0.7
  node1 <- createNode oscNode "oscillator"
  node2 <- createNode gainNode "gain"

  connect node1 node2

  connectToDestination node2
  return ()

---- EJEMPLO 3: Conectar un nodo a un parámetro ----

-- En JS:
-- FUENTE: https://developer.mozilla.org/en-US/docs/Web/API/AudioNode/connect#audioparam_example
-- const audioCtx = new AudioContext();

-- create an normal oscillator to make sound
-- const oscillator = audioCtx.createOscillator();

-- create a second oscillator that will be used as an LFO (Low-frequency
-- oscillator), and will control a parameter
-- const lfo = audioCtx.createOscillator();

-- set the frequency of the second oscillator to a low number
-- lfo.frequency.value = 2.0; 2Hz: -- two oscillations per second

-- create a gain whose gain AudioParam will be controlled by the LFO
-- const gain = audioCtx.createGain();

-- connect the LFO to the gain AudioParam. This means the value of the LFO
-- will not produce any audio, but will change the value of the gain instead
-- lfo.connect(gain.gain);

-- connect the oscillator that will produce audio to the gain
-- oscillator.connect(gain);

-- connect the gain to the destination so we hear sound
-- gain.connect(audioCtx.destination);

-- start the oscillator that will produce audio
-- oscillator.start();

-- start the oscillator that will modify the gain value
-- lfo.start();

-- En Haskell:
connectParamExample :: WebAudioApiGraphDrawer ()
connectParamExample = do
  let oscNode = createOscillatorNode 440 0 Sine
  let lfoNode = createOscillatorNode 2 0 Sine
  let gainNode = createGainNode 1.0
  oscillator <- createNode oscNode "oscillator"
  gain <- createNode gainNode "gain"
  lfo <- createNode lfoNode "lfo"

  gainParam <- getAudioParam gain GainParam  "gainParam"

  -- LIMITACION: Hace falta pasar el nodo padre para poder obtener el parámetro
  -- POSIBLE SOLUCION: Tener referencia al nodo padre en el nodo hijo (algun identificador)
  connectToParam lfo gain gainParam 

  connect oscillator gain
  connectToDestination gain

  startOscillatorNode oscillator
  startOscillatorNode lfo
  return ()

exampleUsage :: WebAudioApiGraphDrawer ()
exampleUsage = do
  let oscNode = createOscillatorNode 0.5 1.5 Sine
  let gainNode = createGainNode 0.7
  node1 <- createNode oscNode "oscillator"
  node2 <- createNode gainNode "gain"

  connect node1 node2

  connectToDestination node2
  connectToDestination node1
  return ()

main :: IO FilePath
main = do
  execute "./output/oscillator" oscillatorEx1
  execute "./output/connect" connectExample
  execute "./output/connectParam" connectParamExample