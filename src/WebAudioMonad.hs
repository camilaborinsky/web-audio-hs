-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebAudioMonad where

import Control.Monad.Writer

data AudioNode
  = OscillatorNode
  | GainNode Double
  | DelayNode Double

newtype WebAudioApiCompiler a = WebAudioApiCompiler {runWebAudioApiCompiler :: Writer String a}
  deriving (Functor, Applicative, Monad, MonadWriter String)

-- Define the Monad type class

-- instance Functor WebAudioApiCompiler where
--   fmap :: (a -> b) -> WebAudioApiCompiler a -> WebAudioApiCompiler b
--   fmap f (WebAudioApiCompiler w) = WebAudioApiCompiler (fmap f w)

-- instance Applicative WebAudioApiCompiler where
--   pure :: a -> WebAudioApiCompiler a
--   pure = WebAudioApiCompiler . pure

--   (<*>) :: WebAudioApiCompiler (a -> b) -> WebAudioApiCompiler a -> WebAudioApiCompiler b
--   (WebAudioApiCompiler wf) <*> (WebAudioApiCompiler wa) = WebAudioApiCompiler (wf <*> wa)

-- instance Monad WebAudioApiCompiler where
--   (WebAudioApiCompiler wa) >>= f = WebAudioApiCompiler (wa >>= runWebAudioApiCompiler . f)

-- instance MonadWriter [Char] WebAudioApiCompiler where
--   writer :: (a, [Char]) -> WebAudioApiCompiler a
--   writer = WebAudioApiCompiler . writer

--   tell :: [Char] -> WebAudioApiCompiler ()
--   tell = WebAudioApiCompiler . tell

--   listen :: WebAudioApiCompiler a -> WebAudioApiCompiler (a, [Char])
--   listen (WebAudioApiCompiler m) = WebAudioApiCompiler $ do
--     (a, w) <- listen m
--     return (a, w)

--   pass :: WebAudioApiCompiler (a, [Char] -> [Char]) -> WebAudioApiCompiler a
--   pass (WebAudioApiCompiler m) = WebAudioApiCompiler $ pass m

-- data WebAudioApiCompiler a = WebAudioApiCompiler (Writer [Char] a)

-- execWriter . (runCompiler (createNode OscillatorNode "oscillator"))

class Monad m => WebAudioMonad m where
  -- Add functions specific to WebAudioApi here

  -- createContext :: [Char] -> m AudioContext

  -- Create an audio node
  createNode :: AudioNode -> [Char] -> m AudioNode

  -- TODO: HACER MAS GENERICA, O ASEGURARSE QUE EL GRAFO TMB SEA [Char]
  execute :: m a -> [Char]

instance WebAudioMonad WebAudioApiCompiler where
  createNode audioNode varName = WebAudioApiCompiler $ do
    let jsCode = generateCreateNewNode audioNode varName
    tell jsCode
    return audioNode

  execute = execWriter . runWebAudioApiCompiler

generateCreateNewNode :: AudioNode -> [Char] -> [Char]
generateCreateNewNode OscillatorNode varName = "const " ++ varName ++ " = new OscillatorNode(audioContext);\n"