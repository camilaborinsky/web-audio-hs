{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module WebAudioApiJS where

import AudioContext
import AudioNode
import AudioParam
import Control.Monad.Writer
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import JS.Encode
import JavaScript
import Var
import WebAudioMonad

-- TODO: MUCHO CODIGO REPETIDO
instance JavaScript AudioNode where
  showJSInit (Oscillator oscNode) =
    let jsonParams = unpack . decodeUtf8 . BL.toStrict $ encode oscNode
     in "new OscillatorNode(" ++ varName globalAudioContext ++ ", " ++ jsonParams ++ ")"
  showJSInit (Gain gainNode) =
    let jsonParams = unpack . decodeUtf8 . BL.toStrict $ encode gainNode
     in "new GainNode(" ++ varName globalAudioContext ++ ", " ++ jsonParams ++ ")"

-- showJSInit (Delay delayNode) =
--   let jsonParams = encode delayNode
--    in "new DelayNode(" show AudioContext ++ ", " show jsonParams ++ ")"

newtype WebAudioApiJS a = WebAudioApiJS {runWebAudioApiJS :: Writer String a}
  deriving (Functor, Applicative, Monad, MonadWriter String)

instance WebAudioMonad WebAudioApiJS where
  createNode audioNode varName = WebAudioApiJS $ do
    let nodeVar = NamedVar {varName = varName, varValue = audioNode}
    let jsCode = compileVariableInit nodeVar
    tell jsCode
    return nodeVar

  getAudioParam audioNodeVar paramType paramVarName = do
    let param = extractParamFromAudioNodeVar audioNodeVar paramType
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

  -- TODO: MEJORAR?
  execute filePath compiler = do
    let jsCode = execWriter $ runWebAudioApiJS compiler
    let audioContextInit = compileVariableInit globalAudioContext
    let finalJsCode = audioContextInit ++ jsCode
    writeFile (filePath ++ ".js") finalJsCode
    return filePath
