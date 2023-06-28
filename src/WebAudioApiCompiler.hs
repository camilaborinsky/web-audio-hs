{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebAudioApiCompiler where

import AudioNode
import Control.Monad.Writer
import JavaScript
import Var
import WebAudioMonad

-- TODO: Generalizar el parametro de options, ya que todos se inicializan igual
instance JavaScript AudioNode where
  showJSInit (OscillatorNode context) = "new OscillatorNode(" ++ varName context ++ ")"
  showJSInit (GainNode gain context) = "new GainNode(" ++ varName context ++ ", {gain: " ++ show gain ++ "})"
  showJSInit (DelayNode delay context) = "new DelayNode(" ++ varName context ++ ", {delayTime: " ++ show delay ++ "})"

instance JavaScript AudioContext where
  showJSInit AudioContext = "new AudioContext()"

newtype WebAudioApiCompiler a = WebAudioApiCompiler {runWebAudioApiCompiler :: Writer String a}
  deriving (Functor, Applicative, Monad, MonadWriter String)

instance WebAudioMonad WebAudioApiCompiler where
  createContext context = do
    let jsCode = compileVariableInit context
    tell jsCode
    return context

  createNode audioNode = WebAudioApiCompiler $ do
    let jsCode = compileVariableInit audioNode
    tell jsCode
    return audioNode

  connect sourceNode destNode = WebAudioApiCompiler $ do
    let jsCode = varName sourceNode ++ ".connect(" ++ varName destNode ++ ");\n"
    tell jsCode
    return ()

  disconnect sourceNode destNode = WebAudioApiCompiler $ do
    let jsCode = varName sourceNode ++ ".disconnect(" ++ varName destNode ++ ");\n"
    tell jsCode
    return ()

  -- TODO: MEJORAR?
  execute compiler = do
    let jsCode = execWriter $ runWebAudioApiCompiler compiler
    let filePath = "./tmp/output.js" -- choose a file path
    writeFile filePath jsCode
    return filePath