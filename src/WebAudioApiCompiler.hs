{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WebAudioApiCompiler where

import WebAudioMonad
import Control.Monad.Writer

newtype WebAudioApiCompiler a = WebAudioApiCompiler {runWebAudioApiCompiler :: Writer String a}
  deriving (Functor, Applicative, Monad, MonadWriter String)

instance WebAudioMonad WebAudioApiCompiler where
  createNode audioNode varName = WebAudioApiCompiler $ do
    let jsCode = generateCreateNewNode audioNode varName
    tell jsCode
    return audioNode

  execute = execWriter . runWebAudioApiCompiler

generateCreateNewNode :: AudioNode -> [Char] -> [Char]
generateCreateNewNode OscillatorNode varName = "const " ++ varName ++ " = new OscillatorNode(audioContext);\n"