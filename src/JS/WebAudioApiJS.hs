{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module JS.WebAudioApiJS
  ( WebAudioApiJS (..),
  )
where

import Control.Monad.Writer
import JS.Compiler
import Var
import WebAudio.AudioGraph
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
    let (AudioNode audioNode) = varValue audioNodeVar
    let newNode = updateAudioParamInNode audioNode param
    let jsCode = compileSetAudioParam audioNodeVar param
    tell jsCode
    return $ NamedVar {varName = varName audioNodeVar, varValue = newNode}

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

  execute filePath compiler = do
    let jsCode = execWriter $ runWebAudioApiJS compiler
    let audioContextInit = compileVariableInit globalAudioContext
    let finalJsCode = audioContextInit ++ jsCode
    writeFile (filePath ++ ".js") finalJsCode
    return filePath
