{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.WebAudioApiJS
  ( WebAudioApiJS (..),
  )
where

import Control.Monad.Writer
import Data.Aeson
import JS.Compiler
import Var
import WebAudio.Types
import WebAudio.WebAudioMonad

newtype WebAudioApiJS a = WebAudioApiJS {runWebAudioApiJS :: Writer String a}
  deriving (Functor, Applicative, Monad, MonadWriter String)

instance WebAudioMonad WebAudioApiJS where
  createNode audioNode varName = WebAudioApiJS $ do
    let nodeVar = NamedVar {varName = varName, varValue = audioNode}
    let jsCode = compileVariableInit nodeVar
    tell jsCode
    return nodeVar

  getAudioParam audioNodeVar paramType paramVarName = do
    let param = extractParamFromAudioNode (varValue audioNodeVar) paramType
    let jsCode = compileGetAudioParam audioNodeVar paramType paramVarName
    tell jsCode
    return $ NamedVar {varName = paramVarName, varValue = param}

  setAudioParam audioNodeVar param = do
    let newNode = updateAudioParamInAudioNodeVar audioNodeVar param
    let jsCode = compileSetAudioParam audioNodeVar param
    tell jsCode
    return newNode

  connect sourceNodeVar destNodeVar = WebAudioApiJS $ do
    let jsCode = varName sourceNodeVar ++ ".connect(" ++ varName destNodeVar ++ ");\n"
    tell jsCode
    return ()

  connectToParam sourceNodeVar _ destParamVar = WebAudioApiJS $ do
    let jsCode = varName sourceNodeVar ++ ".connect(" ++ varName destParamVar ++ ");\n"
    tell jsCode
    return ()

  disconnectFromParam sourceNodeVar _ destParamVar = WebAudioApiJS $ do
    let jsCode = varName sourceNodeVar ++ ".disconnect(" ++ varName destParamVar ++ ");\n"
    tell jsCode
    return ()

  disconnect sourceNodeVar destNodeVar = WebAudioApiJS $ do
    let jsCode = varName sourceNodeVar ++ ".disconnect(" ++ varName destNodeVar ++ ");\n"
    tell jsCode
    return ()

  connectToDestination sourceNodeVar = WebAudioApiJS $ do
    let jsCode = varName sourceNodeVar ++ ".connect(" ++ varName globalAudioContext ++ ".destination);\n"
    tell jsCode
    return ()

  startOscillatorNodeAtTime (NamedVar varName (Oscillator oscNode)) time = WebAudioApiJS $ do
    let jsCode = varName ++ ".start(" ++ show time++ ");\n"
    tell jsCode
    return ()

  startOscillatorNodeAtTime _ _ = do
    error "startOscillatorNode: not an oscillator node"
    return ()
  
  stopOscillatorNodeAtTime (NamedVar varName (Oscillator oscNode)) time = WebAudioApiJS $ do
    let jsCode = varName ++ ".stop("++ show time ++");\n"
    tell jsCode
    return ()
    
  stopOscillatorNodeAtTime _ _= do
    error "stopOscillatorNode: not an oscillator node"
    return ()

  setValueAtTime paramVar value time = WebAudioApiJS $ do
    let jsCode = varName paramVar ++ ".setValueAtTime(" ++ show value ++ ", " ++ show time ++ ");\n"
    tell jsCode
    return ()

  linearRampToValueAtTime paramVar value time = WebAudioApiJS $ do
    let jsCode = varName paramVar ++ ".linearRampToValueAtTime(" ++ show value ++ ", " ++ show time ++ ");\n"
    tell jsCode
    return ()
  
  exponentialRampToValueAtTime paramVar value time = WebAudioApiJS $ do
    let jsCode = varName paramVar ++ ".exponentialRampToValueAtTime(" ++ show value ++ ", " ++ show time ++ ");\n"
    tell jsCode
    return ()
  
  setTargetAtTime paramVar target time constant = WebAudioApiJS $ do
    let jsCode = varName paramVar ++ ".setTargetAtTime(" ++ show target ++ ", " ++ show time ++ ", " ++ show constant ++ ");\n"
    tell jsCode
    return ()

  setValueCurveAtTime paramVar values time duration = WebAudioApiJS $ do
    let jsCode = varName paramVar ++ ".setValueCurveAtTime(" ++ show values ++ ", " ++ show time ++ ", " ++ show duration ++ ");\n"
    tell jsCode
    return ()

  cancelScheduledValues paramVar time = WebAudioApiJS $ do
    let jsCode = varName paramVar ++ ".cancelScheduledValues(" ++ show time ++ ");\n"
    tell jsCode
    return ()
  
  cancelAndHoldAtTime paramVar time = WebAudioApiJS $ do
    let jsCode = varName paramVar ++ ".cancelAndHoldAtTime(" ++ show time ++ ");\n"
    tell jsCode
    return ()
     
  execute filePath compiler = do
    let jsCode = execWriter $ runWebAudioApiJS compiler
    let audioContextInit = compileVariableInit globalAudioContext
    let finalJsCode = audioContextInit ++ jsCode
    writeFile (filePath ++ ".js") finalJsCode
    return filePath
