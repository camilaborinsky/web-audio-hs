{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module WebAudioApiGraphDrawer where

import AudioNode
import AudioParam
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

data WebAudioState = WebAudioState
  { audioGraphState :: M.Map String [String], -- adjacency list
    audioNodeMap :: M.Map String AudioNodeVar -- map from varName to AudioNodeVar
  }

newtype WebAudioApiGraphDrawer a = WebAudioApiGraphDrawer {runWebAudioApiGraphDrawer :: State WebAudioState a}
  deriving (Functor, Applicative, Monad, MonadState WebAudioState)

instance WebAudioMonad WebAudioApiGraphDrawer where
  createNode node varName = do
    let audioNodeVar = NamedVar {varName = varName, varValue = node}
    modify $ \s ->
      let graphState' = M.insert varName [] (audioGraphState s)
          nodeMap' = M.insert varName audioNodeVar (audioNodeMap s)
       in WebAudioState graphState' nodeMap'
    return audioNodeVar

  connect sourceNode destNode = do
    modify $ \s ->
      let graphState' = M.insertWith (++) (varName sourceNode) [varName destNode] (audioGraphState s)
       in s {audioGraphState = graphState'}
    return ()

  disconnect sourceNode destNode = do
    modify $ \s ->
      let graphState' = M.adjust (Prelude.filter (/= varName destNode)) (varName sourceNode) (audioGraphState s)
       in s {audioGraphState = graphState'}
    return ()

  setAudioParam audioNodeVar newParam = do
    WebAudioState graphState nodeMap <- get
    let updatedNodeVar = updateAudioParamInAudioNodeVar audioNodeVar newParam
    let updatedNodeMap = M.insert (varName audioNodeVar) updatedNodeVar nodeMap
    put $ WebAudioState graphState updatedNodeMap
    return updatedNodeVar

  -- TODO: CUIDADO QUE M.! no es safe, puede morir en runtime, la otra alternativa es lookup que devuelve maybe
  getAudioParam audioNodeVar paramType paramVarName = do
    WebAudioState _ nodeMap <- get
    let audioNode = nodeMap M.! varName audioNodeVar
    let param = extractParamFromAudioNodeVar audioNode paramType
    return NamedVar {varName = paramVarName, varValue = param}

  execute state =
    createImage . digraph' . graphToDotM $
      execState (runWebAudioApiGraphDrawer state) (WebAudioState M.empty M.empty)

-- drawGraph :: PrintDotRepr dg n => dg n -> IO FilePath
-- TODO Poner bien aux
aux :: WebAudioState -> [(String, String)]
aux state = Prelude.concatMap (\(k, vs) -> Prelude.map (k,) vs) $ M.toList (audioGraphState state)

graphToDotM :: WebAudioState -> DotM String ()
graphToDotM state = do
  let nodes = M.elems $ audioNodeMap state
  mapM_ (\n -> node (varName n) [textLabel $ pack $ show (varValue n)]) nodes
  mapM_ (uncurry (-->)) $ aux state

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
executeString state = unpack . printDotGraph . digraph' . graphToDotM $ execState (runWebAudioApiGraphDrawer state) (WebAudioState M.empty M.empty)
