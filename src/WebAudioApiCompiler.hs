{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WebAudioApiCompiler where

import WebAudioMonad
import Control.Monad.Writer

newtype WebAudioApiCompiler a = WebAudioApiCompiler {runWebAudioApiCompiler :: Writer String a}
  deriving (Functor, Applicative, Monad, MonadWriter String)

instance WebAudioMonad WebAudioApiCompiler where
  createNode audioNode = WebAudioApiCompiler $ do
    let jsCode = generateCreateNewNode audioNode
    tell jsCode
    return audioNode

  execute = execWriter . runWebAudioApiCompiler

generateCreateNewNode :: AudioNodeVar -> [Char]
generateCreateNewNode (AudioNodeVar (OscillatorNode, varName)) = "const " ++ varName ++ " = new OscillatorNode(audioContext);\n"