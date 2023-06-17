module WebAudioMonad where

import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- data NodeType = BufferSourceNode | GainNode

-- type NodeId = Int

-- data AudioNode = Node NodeId NodeType

-- type Edge = (NodeId, NodeId)

-- data AudioGraph = AudioGraph [AudioNode] [Edge]

-- Define the Monad type class
class Monad m => WebAudioMonad m where
  -- Add functions specific to WebAudioApi here

  -- Create an audio node
  createNode :: m AudioNode

-- Define a data type for the WebAudioApi monad
newtype WebAudioApiM a = WebAudioApiM {runWebAudioApiM :: IO a}

-- Implement the Monad instance for WebAudioApiM
instance Monad WebAudioApiCompiler where
  return = pure
  (>>=) = bind

instance Monad WebAudioApiGraphDrawer where
  return = pure
  (>>=) = bind



-- Implement the Applicative instance for WebAudioApiM
instance Applicative WebAudioApiM where
  pure = WebAudioApiM . pure
  (<*>) = ap

-- Implement the Functor instance for WebAudioApiM
instance Functor WebAudioApiM where
  fmap f (WebAudioApiM action) = WebAudioApiM (fmap f action)

-- Implement the WebAudioMonad instance for WebAudioApiM
instance WebAudioMonad WebAudioApiM where
  -- Implement functions specific to WebAudioApi here

  -- Create an audio node using the WebAudioApi
  createNode = WebAudioApiM $ do
    -- Code to create an audio node using the WebAudioApi
    -- Your code here
    liftIO $ putStrLn "Creating audio node..." -- Just an example
  
  connect 

-- Example usage
exampleUsage :: WebAudioApiM ()
exampleUsage = do
  node <- createNode

-- Use the audio node for further operations
-- Your code here



-- Plantear las instancias de WebAudioApiCompiler y WebAudioApiGraphDrawer (pensar en return y bind)
-- Pensar la class WebAudioMonad, los métodos que contiene, y los tipos