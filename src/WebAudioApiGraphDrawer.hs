{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module WebAudioApiGraphDrawer where

import AudioNode
import Data.GraphViz
import Data.GraphViz.Printing (PrintDot (..), unqtText)
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Types.Generalised (fromGeneralised)
import Data.GraphViz.Types.Generalised qualified as G
import Data.GraphViz.Types.Graph (toCanonical)
import Data.GraphViz.Types.Monadic
import Data.Text.Lazy
import System.FilePath
import Var
import WebAudioMonad
import Data.Map qualified as M
import Control.Monad.State

instance PrintDot AudioNodeVar where
  unqtDot (NamedVar var audioNode) = unqtText $ pack $ var ++ " [label=" ++ varName audioNode ++ "]"

-- let
--     varLabel = textLabel $ pack var
--     dotNode = DotNode var []
-- in unqtDot dotNode

newtype WebAudioApiGraphDrawer a = WebAudioApiGraphDrawer {runWebAudioApiGraphDrawer :: State (M.Map AudioNodeVar [AudioNodeVar]) a }
  deriving (Functor, Applicative, Monad)

instance WebAudioMonad WebAudioApiGraphDrawer where
  createNode audioNodeVar = WebAudioApiGraphDrawer $ do
    modify $ \graph -> insertWith (++) audioNodeVar [] graph
    node audioNodeVar []
    return audioNodeVar

  connect :: AudioNodeVar -> AudioNodeVar -> WebAudioApiGraphDrawer ()
  connect node1 node2 = WebAudioApiGraphDrawer $ do
    edge node1 node2 []
    return ()

  disconnect node1 node2 = WebAudioApiGraphDrawer $ do
    

  execute = createImage . digraph' . runWebAudioApiGraphDrawer

-- drawGraph :: PrintDotRepr dg n => dg n -> IO FilePath

createImage :: PrintDotRepr dg n => dg n -> IO FilePath
createImage = createImageInDir "./tmp" "ex1" Png

createImageInDir :: PrintDotRepr dg n => FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir d n o g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) o (combine d n)

executeString :: WebAudioApiGraphDrawer a -> [Char]
executeString = unpack . printDotGraph . (fromGeneralised :: G.DotGraph AudioNodeVar -> DotGraph AudioNodeVar) . digraph' . runWebAudioApiGraphDrawer
