{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module WebAudioApiJS where

import AudioContext
import AudioNode
import Control.Monad.Writer
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
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

  -- setAudioParam audioNode audioParam = WebAudioApiJS $ do
  --   let jsCode = varName audioNode ++ "." ++ compileAudioParam audioParam ++ ";\n"
  --   tell jsCode
  --   return ()

  connect sourceNode destNode = WebAudioApiJS $ do
    let jsCode = varName sourceNode ++ ".connect(" ++ varName destNode ++ ");\n"
    tell jsCode
    return ()

  disconnect sourceNode destNode = WebAudioApiJS $ do
    let jsCode = varName sourceNode ++ ".disconnect(" ++ varName destNode ++ ");\n"
    tell jsCode
    return ()

  -- TODO: MEJORAR?
  execute compiler = do
    let jsCode = execWriter $ runWebAudioApiJS compiler
    let audioContextInit = compileVariableInit globalAudioContext
    let finalJsCode = audioContextInit ++ jsCode
    let filePath = "./tmp/output.js" -- TODO: parametrizar
    writeFile filePath finalJsCode
    return filePath
