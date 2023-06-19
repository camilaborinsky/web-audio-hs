module WebAudioMonad (WebAudioApiCompiler) where

import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Writer.Lazy (Writer, runWriter, writer)

data AudioNode
  = OscillatorNode
  | GainNode Double
  | DelayNode Double

-- Define the Monad type class
class Monad m => WebAudioMonad m where
  -- Add functions specific to WebAudioApi here

  -- createContext :: String -> m AudioContext

  -- Create an audio node
  createNode :: AudioNode -> String -> m AudioNode

  execute :: m a -> b

newtype WebAudioApiCompiler a = WebAudioApiCompiler {runCompiler :: Writer String a}
  deriving (Functor, Applicative, Monad, MonadWriter String)

instance Monad WebAudioApiCompiler where
  return = WebAudioApiCompiler
  (WebAudioApiCompiler value) >>= f = f value

-- data WebAudioApiCompiler a = WebAudioApiCompiler (Writer String a)

-- execWriter . (runCompiler (createNode OscillatorNode "oscillator"))
instance WebAudioMonad WebAudioApiCompiler where
  createNode audioNode varName = WebAudioApiCompiler $ do
    let jsCode = generateCreateNewNode audioNode varName
    tell jsCode
    return audioNode

  execute (WebAudioApiCompiler writer) = execWriter writer

generateCreateNewNode :: AudioNode -> String -> String
generateCreateNewNode OscillatorNode varName = "const " ++ varName ++ " = new OscillatorNode(audioContext);\n"

-- instance Monad WebAudioApiGraphDrawer where
--   return = pure
--   (>>=) = bind

-- -- Implement the Applicative instance for WebAudioApiM
-- instance Applicative WebAudioApiM where
--   pure = WebAudioApiM . pure
--   (<*>) = ap

-- -- Implement the Functor instance for WebAudioApiM
-- instance Functor WebAudioApiM where
--   fmap f (WebAudioApiM action) = WebAudioApiM (fmap f action)

-- -- Implement the WebAudioMonad instance for WebAudioApiM
-- instance WebAudioMonad WebAudioApiM where
--   -- Implement functions specific to WebAudioApi here

--   -- Create an audio node using the WebAudioApi
--   createNode = WebAudioApiM $ do
--     -- Code to create an audio node using the WebAudioApi
--     -- Your code here
--     liftIO $ putStrLn "Creating audio node..." -- Just an example

--   connect

-- -- Example usage
-- exampleUsage :: WebAudioApiM ()
-- exampleUsage = do
--   node <- createNode

-- Use the audio node for further operations
-- Your code here

-- Plantear las instancias de WebAudioApiCompiler y WebAudioApiGraphDrawer (pensar en return y bind)
-- Pensar la class WebAudioMonad, los métodos que contiene, y los tipos