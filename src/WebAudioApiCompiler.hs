{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebAudioApiCompiler where

import Control.Monad.Writer
import WebAudioMonad

newtype WebAudioApiCompiler a = WebAudioApiCompiler {runWebAudioApiCompiler :: Writer String a}
  deriving (Functor, Applicative, Monad, MonadWriter String)

instance WebAudioMonad WebAudioApiCompiler where
  createNode audioNode = WebAudioApiCompiler $ do
    let jsCode = generateCreateNewNode audioNode
    tell jsCode
    return audioNode

    -- TODO: MEJORAR?
  execute compiler = do
    let jsCode = execWriter $ runWebAudioApiCompiler compiler
    let filePath = "./tmp/output.js" -- choose a file path
    writeFile filePath jsCode
    return filePath

generateCreateNewNode :: AudioNodeVar -> [Char]
generateCreateNewNode (AudioNodeVar (OscillatorNode, varName)) = "const " ++ varName ++ " = new OscillatorNode(audioContext);\n"