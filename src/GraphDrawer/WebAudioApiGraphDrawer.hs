{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module GraphDrawer.WebAudioApiGraphDrawer where

import Control.Monad.State
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing (PrintDot (..), unqtText)
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Types.Generalised (fromGeneralised)
import Data.GraphViz.Types.Generalised qualified as G
import Data.GraphViz.Types.Graph (toCanonical)
import Data.GraphViz.Types.Monadic
import Data.List (partition)
import Data.Map qualified as M
import Data.Text.Lazy (Text (..), pack, unpack)
import System.FilePath
import Var
import WebAudio.Types
import WebAudio.WebAudioMonad

instance PrintDot AudioNodeVar where
  unqtDot audioNode = unqtText $ pack $ varName audioNode

data AudioGraphNodeType = Node | Param
  deriving (Eq, Show, Ord)

newtype AudioGraphNodeRef = AudioGraphNodeRef (AudioGraphNodeType, String) -- (nodeType, varName)
  deriving (Eq, Ord)

isAudioNodeRef :: AudioGraphNodeRef -> Bool
isAudioNodeRef (AudioGraphNodeRef (Node, _)) = True
isAudioNodeRef _ = False

createGraphNodeRef :: AudioGraphNodeType -> String -> AudioGraphNodeRef
createGraphNodeRef nodeType varName = AudioGraphNodeRef (nodeType, varName)

getParamNodeId :: AudioNodeVar -> AudioParam -> String
getParamNodeId audioNodeVar param = varName audioNodeVar ++ "-" ++ show param

data WebAudioState = WebAudioState
  { audioGraphState :: M.Map AudioGraphNodeRef [AudioGraphNodeRef], -- adjacency list
    audioNodeMap :: M.Map String AudioNodeVar -- map from varName to AudioNodeVar
  }

newtype WebAudioApiGraphDrawer a = WebAudioApiGraphDrawer {runWebAudioApiGraphDrawer :: State WebAudioState a}
  deriving (Functor, Applicative, Monad, MonadState WebAudioState)

instance WebAudioMonad WebAudioApiGraphDrawer where
  createNode node varName = do
    let audioNodeVar = NamedVar {varName = varName, varValue = node}
    modify $ \s ->
      let graphState' = M.insert (createGraphNodeRef Node varName) [] (audioGraphState s)
          nodeMap' = M.insert varName audioNodeVar (audioNodeMap s)
          params = Prelude.map (createGraphNodeRef Param . getParamNodeId audioNodeVar) $ getParamsFromAudioNode node
          graphState'' = Prelude.foldl (\acc paramRef -> M.insert paramRef [] acc) graphState' params
       in WebAudioState graphState'' nodeMap'
    return audioNodeVar

  connect sourceNodeVar destNodeVar = do
    modify $ \s ->
      let graphState' = M.insertWith (++) (createGraphNodeRef Node $ varName sourceNodeVar) [createGraphNodeRef Node $ varName destNodeVar] (audioGraphState s)
       in s {audioGraphState = graphState'}
    return ()

  disconnect sourceNodeVar destNodeVar = do
    modify $ \s ->
      let graphState' =
            M.adjust
              (Prelude.filter (/= (createGraphNodeRef Node $ varName destNodeVar)))
              (createGraphNodeRef Node $ varName sourceNodeVar)
              (audioGraphState s)
       in s {audioGraphState = graphState'}
    return ()

  connectToParam sourceNodeVar destNodeVar destParamVar = do
    modify $ \s ->
      let graphState' = M.insertWith (++) (createGraphNodeRef Node $ varName sourceNodeVar) [createGraphNodeRef Param $ getParamNodeId destNodeVar $ varValue destParamVar] (audioGraphState s)
       in s {audioGraphState = graphState'}
    return ()

  disconnectFromParam sourceNodeVar destNodeVar destParamVar = do
    modify $ \s ->
      let graphState' =
            M.adjust
              (Prelude.filter (/= (createGraphNodeRef Param $ getParamNodeId destNodeVar $ varValue destParamVar)))
              (createGraphNodeRef Node $ varName sourceNodeVar)
              (audioGraphState s)
       in s {audioGraphState = graphState'}
    return ()

  setAudioParam audioNodeVar newParam = do
    WebAudioState graphState nodeMap <- get
    let updatedNodeVar = updateAudioParamInAudioNodeVar audioNodeVar newParam
    let updatedNodeMap = M.insert (varName audioNodeVar) updatedNodeVar nodeMap
    put $ WebAudioState graphState updatedNodeMap
    return updatedNodeVar

  getAudioParam audioNodeVar paramType paramVarName = do
    let param = extractParamFromAudioNode (varValue audioNodeVar) paramType
    return NamedVar {varName = paramVarName, varValue = param}

  startOscillatorNodeAtTime (NamedVar varName (Oscillator oscNode)) _ = return ()
  startOscillatorNodeAtTime _ _ = do
    error "startOscillatorNode: node is not an oscillator"
    return ()

  stopOscillatorNodeAtTime (NamedVar varName (Oscillator oscNode)) _ = return ()
  stopOscillatorNodeAtTime _ _ = do
    error "stopOscillatorNode: node is not an oscillator"
    return ()

  setValueAtTime _ _ _ = return ()

  linearRampToValueAtTime _ _ _ = return ()

  exponentialRampToValueAtTime _ _ _ = return ()

  setTargetAtTime _ _ _ _ = return ()

  setValueCurveAtTime _ _ _ _ = return ()

  cancelScheduledValues _ _ = return ()

  cancelAndHoldAtTime _ _ = return ()

  connectToDestination audioNodeVar = do
    node <- createNode Destination "destination"
    connect audioNodeVar node
    return ()

  execute filePath state =
    createImage filePath . digraph' . graphToDotM $
      execState (runWebAudioApiGraphDrawer state) (WebAudioState M.empty M.empty)

-- drawGraph :: PrintDotRepr dg n => dg n -> IO FilePath
getAudioGraphEdges :: WebAudioState -> [(AudioGraphNodeRef, AudioGraphNodeRef)]
getAudioGraphEdges state = Prelude.concatMap (\(k, vs) -> Prelude.map (k,) vs) $ M.toList (audioGraphState state)

-- this is a hack for connecting clusters through invisible nodes using the ltail and lhead attributes
-- the library automatically prepends "cluster_" to all cluster ids, so we need to add it manually
-- for the ltail and lhead attributes to work
getClusterID :: AudioGraphNodeRef -> Text
getClusterID (AudioGraphNodeRef (Node, varName)) = pack $ "cluster_" ++ varName
getClusterID _ = error "getClusterID: node type is not a cluster"

graphToDotM :: WebAudioState -> DotM String ()
graphToDotM state = do
  graphAttrs [Compound True, RankDir FromLeft] -- set the global graph attributes for connecting clusters, make it horizontal with rankdir
  let nodes = M.elems $ audioNodeMap state
  mapM_
    ( \n -> do
        let nodeName = varName n
        cluster (textGraphID $ pack nodeName) $ do
          -- each audio node is represented as a cluster
          graphAttrs [textLabel $ pack $ getAudioNodeType (varValue n) ++ "\n" ++ varName n] -- set the label for the cluster
          let params = getParamsFromAudioNode $ varValue n
          mapM_
            ( \param ->
                let paramId = getParamNodeId n param
                 in node paramId [textLabel $ pack $ show param]
            )
            params -- create a node for each param
            -- create an invisible node to connect the cluster to the rest of the graph
            -- TODO: HACER CONSTANTE CENTRALIZADA DEL -invisible
          let invisibleNodeId = nodeName ++ "-invisible"
          node invisibleNodeId [Shape PlainText, Label (StrLabel ""), Width 0, Height 0]
    )
    nodes

  let edges = getAudioGraphEdges state

  -- do hack to connect clusters through invisible nodes
  let (audioNodeEdges, audioParamEdges) = partition (isAudioNodeRef . snd) edges

  forM_ audioNodeEdges $ \(sourceNodeRef@(AudioGraphNodeRef (_, sourceName)), destNodeRef@(AudioGraphNodeRef (_, destName))) -> do
    edge
      (sourceName ++ "-invisible")
      (destName ++ "-invisible")
      [LTail $ getClusterID sourceNodeRef, LHead $ getClusterID destNodeRef]

  forM_ audioParamEdges $ \(sourceNodeRef@(AudioGraphNodeRef (_, sourceName)), destNodeRef@(AudioGraphNodeRef (_, destName))) -> do
    edge (sourceName ++ "-invisible") destName [LTail $ getClusterID sourceNodeRef]

createImage :: PrintDotRepr dg n => String -> dg n -> IO FilePath
createImage filePath = createImageInDir filePath Png

createImageInDir :: PrintDotRepr dg n => FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir filePath outputType graph = Data.GraphViz.addExtension (runGraphvizCommand Dot graph) outputType filePath

executeString :: WebAudioApiGraphDrawer a -> [Char]
executeString state = unpack . printDotGraph . digraph' . graphToDotM $ execState (runWebAudioApiGraphDrawer state) (WebAudioState M.empty M.empty)
