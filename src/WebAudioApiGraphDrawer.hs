{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WebAudioApiGraphDrawer where

import Data.GraphViz
import Data.GraphViz.Printing (PrintDot (..), unqtText)
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Types.Generalised (fromGeneralised)
import qualified Data.GraphViz.Types.Generalised as G
import Data.GraphViz.Types.Graph (toCanonical)
import Data.GraphViz.Types.Monadic
import Data.Text.Lazy
import System.FilePath
import WebAudioMonad
-- import Text.PrettyPrint.Leijen.Text (text, (<+>), brackets)

instance PrintDot AudioNodeVar where
  unqtDot (AudioNodeVar (audioNode, var)) = unqtText (pack var)
    --  text (pack var) <+> brackets (toDot $ textLabel $ pack $ show audioNode ++ " " ++ var)
    -- let 
    --     varLabel = textLabel $ pack var
    --     dotNode = DotNode var []
    -- in unqtDot dotNode

newtype WebAudioApiGraphDrawer a = WebAudioApiGraphDrawer {runWebAudioApiGraphDrawer :: DotM AudioNodeVar a}
  deriving (Functor, Applicative, Monad)

instance WebAudioMonad WebAudioApiGraphDrawer where
  createNode audioNodeVar = WebAudioApiGraphDrawer $ do
    graphNode <- node audioNodeVar []
    return audioNodeVar


  execute = createImage  . digraph' . runWebAudioApiGraphDrawer

-- drawGraph :: PrintDotRepr dg n => dg n -> IO FilePath

createImage :: PrintDotRepr dg n =>  dg n -> IO FilePath
createImage = createImageInDir "./tmp" "ex1" Png 

createImageInDir :: PrintDotRepr dg n => FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir d n o g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) o (combine d n)


executeString :: WebAudioApiGraphDrawer a -> [Char]
executeString = unpack . printDotGraph . (fromGeneralised :: G.DotGraph AudioNodeVar -> DotGraph AudioNodeVar) . digraph' . runWebAudioApiGraphDrawer 


