{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module WebAudioApiGraphDrawer where

import AudioNode
import Control.Monad.State
import Data.GraphViz
import Data.GraphViz.Printing (PrintDot (..), unqtText)
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Types.Generalised (fromGeneralised)
import Data.GraphViz.Types.Generalised qualified as G
import Data.GraphViz.Types.Graph (toCanonical)
import Data.GraphViz.Types.Monadic
import Data.Map qualified as M
import Data.Text.Lazy
import System.FilePath
import Var
import WebAudioMonad

instance PrintDot AudioNodeVar where
  unqtDot audioNode = unqtText $ pack $ varName audioNode

-- unqtDot audioNode = unqtText $ pack $ varName audioNode ++ " [label=" ++ show (varValue audioNode) ++ "]"

-- let
--     varLabel = textLabel $ pack var
--     dotNode = DotNode var []
-- in unqtDot dotNode

type AudioGraphState = M.Map AudioNodeVar [AudioNodeVar]

newtype WebAudioApiGraphDrawer a = WebAudioApiGraphDrawer {runWebAudioApiGraphDrawer :: State AudioGraphState a}
  deriving (Functor, Applicative, Monad, MonadState AudioGraphState)

instance WebAudioMonad WebAudioApiGraphDrawer where
  createContext audioContextVar = do
    return audioContextVar
  createNode audioNodeVar = do
    modify $ M.insert audioNodeVar []
    return audioNodeVar

  connect sourceNode destNode = do
    modify $ M.insertWith (++) sourceNode [destNode]
    return ()

  disconnect sourceNode destNode = do
    modify $ M.adjust (Prelude.filter (/= destNode)) sourceNode
    return ()

  execute state = createImage . digraph' . graphToDotM $ execState (runWebAudioApiGraphDrawer state) M.empty

-- drawGraph :: PrintDotRepr dg n => dg n -> IO FilePath
-- TODO Poner bien aux
aux graph = Prelude.concatMap (\(k, vs) -> Prelude.map (k,) vs) $ M.toList graph

graphToDotM :: AudioGraphState -> DotM AudioNodeVar ()
graphToDotM graph = do
  mapM_ (\n -> node n [textLabel $ pack $ show (varValue n)]) $ M.keys graph
  mapM_ (uncurry (-->)) $ aux graph

-- let edges = Prelude.concatMap (\(k, vs) -> Prelude.map (k,) vs) $ M.toList graph
-- mapM_ (uncurry -->) edges

-- graphToDotM :: AudioGraphState -> DotM AudioNodeVar ()
-- graphToDotM graph = do
-- map graph keys to dot nodes

-- graphToDotM :: AudioGraphState -> DotM AudioNodeVar ()
-- graphToDotM graph = do
--   Prelude.mapM (\(node, nodes) -> Prelude.mapM (node -->) nodes) $ M.toList graph

createImage :: PrintDotRepr dg n => dg n -> IO FilePath
createImage = createImageInDir "./tmp" "ex1" Png

createImageInDir :: PrintDotRepr dg n => FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir d n o g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) o (combine d n)

executeString :: WebAudioApiGraphDrawer a -> [Char]
executeString state = unpack . printDotGraph . digraph' . graphToDotM $ execState (runWebAudioApiGraphDrawer state) M.empty
