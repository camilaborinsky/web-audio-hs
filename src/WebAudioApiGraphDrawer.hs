{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WebAudioApiGraphDrawer where

import WebAudioMonad
import Control.Monad.Writer

newtype WebAudioApiGraphDrawer a = WebAudioApiGraphDrawer {runWebAudioApiGraphDrawer :: Writer String a}
  deriving (Functor, Applicative, Monad, MonadWriter String)

instance WebAudioMonad WebAudioApiGraphDrawer where
  createNode audioNode varName = WebAudioApiGraphDrawer $ do
   
    return audioNode

  execute = execWriter . runWebAudioApiGraphDrawer