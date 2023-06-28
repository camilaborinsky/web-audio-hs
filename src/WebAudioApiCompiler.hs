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

  connect node1 node2 = WebAudioApiCompiler $ do
    let jsCode = varName node1 ++ ".connect(" ++ varName node2 ++ ");\n"
    tell jsCode
    return ()

  disconnect node1 node2 = WebAudioApiCompiler $ do
    let jsCode = varName node1 ++ ".disconnect(" ++ varName node2 ++ ");\n"
    tell jsCode
    return ()

  -- TODO: MEJORAR?
  execute compiler = do
    let jsCode = execWriter $ runWebAudioApiCompiler compiler
    let filePath = "./tmp/output.js" -- choose a file path
    writeFile filePath jsCode
    return filePath