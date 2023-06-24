{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebAudioApiGraphDrawer where

import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Canonical
import Data.GraphViz
import Data.GraphViz.Types.Monadic 
import Data.GraphViz.Types.Graph (toCanonical)
import Data.GraphViz.Types.Generalised (fromGeneralised)
import WebAudioMonad
import Data.Text.Lazy 

import Data.GraphViz.Printing (PrintDot(..))

instance PrintDot AudioNodeVar where
  unqtDot (AudioNodeVar (audioNode, var)) =
    let labelAttr = textLabel (pack var)
        dotNode = DotNode var [labelAttr]
    in unqtDot dotNode

newtype WebAudioApiGraphDrawer a = WebAudioApiGraphDrawer {runWebAudioApiGraphDrawer :: DotM AudioNodeVar a}
  deriving (Functor, Applicative, Monad)

instance WebAudioMonad WebAudioApiGraphDrawer where
  createNode audioNodeVar = WebAudioApiGraphDrawer $ do
    graphNode <- node audioNodeVar []
    return audioNodeVar

  execute = unpack . printDotGraph . (fromGeneralised :: G.DotGraph AudioNodeVar -> DotGraph AudioNodeVar) . digraph' . runWebAudioApiGraphDrawer 

-- drawGraph :: PrintDotRepr dg n => dg n -> IO FilePath