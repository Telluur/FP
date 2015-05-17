import Prelude
import Data.Maybe
import Data.Char (isDigit)
import Data.List ((\\), delete)


import Eventloop.EventloopCore
import Eventloop.DefaultConfiguration
import Eventloop.Types.EventTypes

import qualified Eventloop.Module.Websocket.Canvas as C
import qualified Eventloop.Module.BasicShapes as B
import qualified Eventloop.Module.Websocket.Mouse as M
import qualified Eventloop.Module.Websocket.Keyboard as K
import qualified Eventloop.Module.StdOut as S
import Eventloop.Module.Graphs


{- | Start
This function will start the eventloop system using the eventloopConfig
-}
start = startMainloop eventloopConfig

{- | The configuration of the Eventloop system
Uses the graphs module to display graphs. This module
depends on the Keyboard, Mouse, Canvas and BasicShapes modules
-}
eventloopConfig = defaultConfig { moduleConfigurations=[ defaultGraphsModuleConfiguration
                                                       , B.defaultBasicShapesModuleConfiguration
                                                       , C.defaultCanvasModuleConfiguration
                                                       , M.defaultMouseModuleConfiguration
                                                       , K.defaultKeyboardModuleConfiguration
                                                       , S.defaultStdOutModuleConfiguration
                                                       ]}
                where
                    defaultConfig = allModulesEventloopConfiguration beginProgramState eventloop -- Uses beginProgramState and eventloop to build config


{- | ProgramState
This datatype shows which variables are kept
-}
data ProgramState
    = ProgramState { pressedKey :: [Char]
                   , node1Select :: Maybe Node
                   , node2Select :: Maybe Node
                   , graph :: Graph
                   }


{- | Begingraph
   This is the start state of the graph
-}
beginGraph = Graph allNodes allEdges Directed Unweighted
           where
            allNodes = [ ('a', (50, 50), Yellow)
                       , ('b', (150, 50), Yellow)
                       , ('c', (350, 50), Orange)
                       , ('d', (450, 50), Orange)
                       , ('e', (200, 200), Purple)
                       , ('f', (300, 200), Green)
                       ]
            allEdges = [ ('a', 'b', Black, 5, Thin)
                       , ('b', 'c', Black, 4, Thin)
                       , ('c', 'd', Black, 3, Thin)
                       , ('f', 'c', Black, 2, Thin)
                       , ('f', 'd', Black, 1, Thin)
                       , ('e', 'a', Black, 2, Thin)
                       , ('e', 'b', Black, 3, Thin)
                       , ('f', 'e', Black, 4, Thin)
                       ]


{-| The beginstate of the ProgramState
-}
beginProgramState = ProgramState [] Nothing Nothing beginGraph


{- | Instructions
This is the list of all possible instructions
Feel free to add your own
-}
instructions = [ "Instructions"
               , "Press 'n', click on the screen to create a new node"
               , "Press 'r', click on a node and press a letter to rename the node"
               , "Press 'e', click on two nodes to create an edge"
               , "Press 'd', click on a node to delete the node"
               , "Press 'w', click on two nodes and press a number to weight the edge in between"
               , "Press 'f', click on two nodes to delete an edge"
               , "Press 'c', click on a node to color it red"
               , "Press 'b', click on a node to color its directed neighbors blue"
               , "Press 'p', Click on two node to check if a path from the first to the second selected nodes"
               , "Press 'l', Click on two nodes to display the paths between the nodes."
               , "Press 'o', Colors all nodes Orange"
               , "Press 'x' Checks whether the graph is strongly connected"
               , "Press 'esc' to abort the current operation and start another"
               ]


{- | A variable showing which labels are used for visually added nodes
-}
automaticPossibleLabels :: [Label]
automaticPossibleLabels = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']


{- | A function to determine which label can be used next
-}
nextLabel :: [Node] -> Label
nextLabel nodes
    | null leftOverLabels = error "Did not have a leftover label to give to a node. Please do not create so many nodes!"
    | otherwise = head leftOverLabels
    where
        currentLabels = map (\(l, _, _) -> l) nodes
        leftOverLabels = automaticPossibleLabels \\ currentLabels


{- | Add a node to the graph
-}
addNode :: Graph -> Node -> Graph
addNode g@(Graph{nodes=ns}) n = g {nodes=(n:ns)}


{- | Add an edge to the graph
-}
addEdge :: Graph -> Edge -> Graph
addEdge g@(Graph{edges=es}) e = g {edges=(e:es)}


{- | Create an edge based on two nodes
Is drawn from node1 to node2
-}
createEdge :: Node -> Node -> Edge
createEdge (l1, _, c) (l2, _, _)
    = (l1, l2, c, 0, Thin)

getNode :: Label -> Graph -> Node
getNode label g  = n
  where
    allNodes = nodes g
    (n:ns) = filter (\(l, _, _) -> l == label ) allNodes


{- | Gets the head node of an edge
Also, funny sex jokes.
-}
getHead :: Edge -> Graph -> Node
getHead  (_, h, _, _, _) g = n
  where
  allNodes = nodes g
  (n:ns) = filter (\(l, _, _) -> h == l) allNodes

{- | Gets the tail node of an edge
-}
getTail :: Edge -> Graph -> Node
getTail (t, _, _, _, _) g = n
  where
  allNodes = nodes g
  (n:ns) = filter (\(l, _, _) -> t == l) allNodes


{- | Finds the edge directed from the first to the second node
-}
findEdgeFromNodeToNode :: Node -> Node -> Graph -> Maybe Edge
findEdgeFromNodeToNode n1 n2 g
    | null possibleEdges = Nothing
    | otherwise = Just $ head possibleEdges
    where
        allEdges = edges g
        possibleEdges = filter (edgeRunsFromNodeToNode n1 n2) allEdges


{- | Finds all edges connected to this node
-}
findEdgesAtNode :: Node -> Graph -> [Edge]
findEdgesAtNode (l, _, _) g
    = filter (\(el1, el2, _, _, _) -> el1 == l || el2 == l) allEdges
    where
        allEdges = edges g

{- | Finds all directed edges connected from this node
-}
findDirectedEdgesAtNode :: Node -> Graph -> [Edge]
findDirectedEdgesAtNode (l, _, _) g
  = filter (\(el1, _, _, _, _) -> el1 == l) allEdges
  where
    allEdges = edges g


{- | Finds all edges that are between two nodes
-}
findEdgesBetweenNodes :: Node -> Node -> Graph -> [Edge]
findEdgesBetweenNodes n1 n2 g
    = filter (edgeIsBetweenNodes n1 n2)  allEdges
    where
        allEdges = edges g


{- | Conditional to check if an edge is connected to both nodes
-}
edgeIsBetweenNodes :: Node -> Node -> Edge -> Bool
edgeIsBetweenNodes (l1, _, _) (l2, _, _) (el1, el2, _, _, _)
    = (el1 == l1 && el2 == l2) || (el1 == l2 && el2 == l1)


{- | Conditional to check if the runs is directed from the first
to the second node
-}
edgeRunsFromNodeToNode :: Node -> Node -> Edge -> Bool
edgeRunsFromNodeToNode (l1, _, _) (l2, _, _) (el1, el2, _, _, _)
    = (l1 == el1) && (l2 == el2)


{- | Removes the node from the graph
-}
removeNode :: Node -> Graph -> Graph
removeNode n g
    = g {nodes = allNodes'}
    where
        allNodes = nodes g
        allNodes' = delete n allNodes

{- | Removes the edge from the graph
-}
removeEdge :: Edge -> Graph -> Graph
removeEdge e g
    = g {edges = allEdges'}
    where
        allEdges = edges g
        allEdges' = delete e allEdges

{- | Removes a node, and all edges connected to it,
from the graph
-}
removeNodeWithAdjoiningEdges :: Node -> Graph -> Graph
removeNodeWithAdjoiningEdges n g
    = g''
    where
        g'  = removeNode n g
        g'' = foldr removeEdge g' (findEdgesAtNode n g)

{- | Rename a node in the edge to the new label
if the node is connected to that edge
-}
renameNodeInEdge :: Node -> Label -> Edge -> Edge
renameNodeInEdge (oldL, _, _) newL (el1, el2, color, weight, thickness)
    | oldL == el1 = (newL, el2, color, weight, thickness)
    | oldL == el2 = (el1, newL, color, weight, thickness)
    | otherwise   = (el1, el2, color, weight, thickness)

neighbors :: Node -> Graph -> [Node]
neighbors node@(l, _, _) g@(Graph{directed=Undirected}) = tails ++ heads
  where
    neighborEdges = findEdgesAtNode node g
    tailEdges = filter (\(t, _, _, _, _) -> t == l) neighborEdges
    headEdges = filter (\(_, h, _, _, _) -> h == l) neighborEdges
    tails = map (\(_, hl, _, _, _) -> (getNode hl g)) tailEdges
    heads = map (\(tl, _, _, _, _) -> (getNode tl g)) headEdges
neighbors node g@(Graph{directed=Directed}) = nb
  where
    nbedges = findDirectedEdgesAtNode node g
    nb = map (flip getHead g) nbedges




{- | Color the (directed) neigbours of a node
-}
colorNeighbors :: Color -> Node -> Graph -> Graph
colorNeighbors newColor node g
  = addNewNodes
  where
    neighborNodes = neighbors node g
    newNeighbors = map (\(l, p, _) -> (l, p, newColor)) neighborNodes
    remOldNodes = foldl (flip removeNode) g neighborNodes
    addNewNodes = foldl (addNode) remOldNodes newNeighbors

{- | colors all nodes to a certain color
-}
colorAll :: Color -> Graph -> Graph
colorAll color g = addNewNodes
  where
    oldNodes = nodes g
    newNodes = map (\(l, p, _) -> (l, p, color)) oldNodes
    remOldNodes = foldl (flip removeNode) g oldNodes
    addNewNodes = foldl (addNode) remOldNodes newNodes

{- | Checks whether a path exists from node to node.
-}
existsPath :: Node -> Node -> Graph -> Bool
existsPath from to g = to `elem` (reachable g from)


{- | Returns a list of nodes which is reachable by node n
-}
reachable :: Graph -> Node -> [Node]
reachable g node = reachable' g [node] (neighbors node g)

reachable' :: Graph -> [Node] -> [Node] -> [Node]
reachable' g rns [] = rns
reachable' g rns nns
  = reachable' g rns' diff'
  where
    nbs = map (flip neighbors g) nns
    rns' = rns ++ nns
    diff = map (reachable'' rns') nbs
    diff' = foldl (++) [] diff

reachable'' :: [Node] -> [Node] -> [Node]
reachable'' rns' nb = filter (flip notElem rns') nb

{- | Checks whether a graph is strongly connected.
-}
isStronglyConnected :: Graph -> Bool
isStronglyConnected g = foldl (&&) True checks'
  where
    allNodes = nodes g
    checks = tuplePairs allNodes
    checks' = map (\(a,b) -> existsPath a b g) checks

{- | Calculates all the paths from 'from' to 'to' excluding loops.
-}
paths :: Graph -> Node -> Node -> [Path]
paths g from to = filter (/=[]) p
  where
    nbs = neighbors from g
    p = foldl (++) [] $ map (paths' g from to [from]) nbs

paths' :: Graph -> Node -> Node -> [Node] -> Node -> [Path]
paths' g from to path next
  | next == to = [path ++ [next]]
  | newnbs == [] = [[]]
  | otherwise = foldl (++) [] $ map (paths' g from to (path ++ [next])) newnbs
  where
    newnbs = filter (flip notElem path) (neighbors next g)

--cheapestPaths :: [(Path, Cost)] -> [(Path, Cost)]
--cheapestPaths (:xs)

--Helper functions
type Path = [Node]
type Cost = Int

tuplePairs :: Eq t => [t] -> [(t, t)]
tuplePairs list = foldl (++) [] (tuplePairs' list list)

tuplePairs' :: Eq t => [t] -> [t] -> [[(t, t)]]
tuplePairs' [] _ = []
tuplePairs' l@(x:xs) list = (map (\a -> (x,a)) gen) : (tuplePairs' xs list)
  where
    gen = filter (/=x) list

prettyPath :: Path -> [Label]
prettyPath path = map (\(l, _, _) -> l) path

prettyPathList :: [Path] -> [[Label]]
prettyPathList pl = map (prettyPath) pl


{- | The eventloop
This function uses the current state and an In event to determine
the new state and what changes should be made as a list of Out events.
-}
eventloop :: ProgramState -> In -> (ProgramState, [Out])

eventloop ps Start
    = (ps, [OutGraphs SetupGraphs, OutGraphs $ DrawGraph (graph ps), OutGraphs $ Instructions instructions])

eventloop ps@(ProgramState "f" (Just node1s) _ g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [])
    | edgeM == Nothing = (ProgramState [] Nothing Nothing g, [])
    | otherwise = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Deleted edge from '" ++ [l1] ++ "' to '" ++ [l2] ++ "'\n"])
    where
        nodeAtPosM = onNode allNodes p
        (Just nodeAtPos) = nodeAtPosM
        allNodes = nodes g
        edgeM = findEdgeFromNodeToNode node1s nodeAtPos g
        (Just edge) = edgeM
        (l1, l2, _, _, _) = edge
        g' = removeEdge edge g

{- | If 'w' has been pressed, two nodes are selected and the next key
is a digit, the edge running from node1s to node2s is weighted as that
digit
-}
eventloop ps@(ProgramState "w" (Just node1s) (Just node2s) g) (InGraphs (Key [key]))
    | isDigit key && edgeM /= Nothing = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Weighted edge from '" ++ [l1] ++ "' to '" ++ [l2] ++ "' with " ++ (show weight) ++ "\n"])
    | otherwise   = (ProgramState [] Nothing Nothing g, [])
    where
        edgeM = findEdgeFromNodeToNode node1s node2s g
        (Just edge@(l1, l2, col, w, thick)) = edgeM
        weight = read [key] :: Weight
        edge' = (l1, l2, col, weight, thick)
        g' =  (flip addEdge) edge' $ removeEdge edge g



{- | If 'd' has been pressed and a node is selected
, the node is deleted from the graph
-}
eventloop ps@(ProgramState "d" _ _ g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [])
    | otherwise = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Deleted node '" ++ [l] ++ "'\n"])
    where
        (l, _, _) = nodeAtPos
        nodeAtPosM = onNode allNodes p
        (Just nodeAtPos) = nodeAtPosM
        allNodes = nodes g
        g' = removeNodeWithAdjoiningEdges nodeAtPos g


{- | If 'e' has been pressed, a node selected and a new node is selected
an edge is drawn between the two nodes
-}
eventloop ps@(ProgramState "e" (Just node1s) _ g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [])
    | otherwise = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Created edge from '" ++ [l1] ++ "' to '" ++ [l2] ++ "'\n"])
    where
        (l1, _, _) = node1s
        (l2, _, _) = nodeAtPos
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g
        (Just nodeAtPos) = nodeAtPosM
        g' = addEdge g $ createEdge node1s nodeAtPos


{- | If 'r' has been pressed, a node selected and a new key stroke
comes in, the label of the selected node is changed
-}
eventloop ps@(ProgramState "r" (Just node1s) _ g) (InGraphs (Key [l]))
    = (ProgramState [] Nothing Nothing g'', [OutGraphs $ DrawGraph g'', OutStdOut $ S.StdOutMessage $ "Renamed node '" ++ [oldL] ++ "' to '" ++ [l] ++ "'\n"])
    where
        allNodes = nodes g
        allEdges = edges g
        (oldL, p, color) = node1s
        node' = (l, p, color)
        allEdges' = map (renameNodeInEdge node1s l) allEdges :: [Edge]
        g'  = (flip addNode) node' $ removeNode node1s g
        g'' = g' {edges = allEdges'}

{- | If 'c' has been pressed and a node selected,
the selected node will be colored red
-}
eventloop ps@(ProgramState "c" _ _ g) (InGraphs (Mouse (Click _) p))
  | nodeAtPosM == Nothing = (ps, [OutStdOut $ S.StdOutMessage "Tried to recolor a node, but failed to select a node"])
  | otherwise = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Colored node '" ++ [label] ++ "' red\n"])
  where
    allNodes = nodes g
    nodeAtPosM = onNode allNodes p
    Just nodeAtPos@(label, pos, color) = nodeAtPosM
    node' = (label, pos, Red)
    g'  = (flip addNode) node' $ removeNode nodeAtPos g


{- | If 'b' has been pressed and a node selected,
the neighbours of the selected node will be colored blue
-}
eventloop ps@(ProgramState "b" _ _ g) (InGraphs (Mouse (Click _) p))
  | nodeAtPosM == Nothing = (ps, [OutStdOut $ S.StdOutMessage "Tried to recolor neighboring nodes, but failed to select a source node"])
  | otherwise = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Colored neighbors of '" ++ [label] ++ "' blue\n"])
  where
    allNodes = nodes g
    nodeAtPosM = onNode allNodes p
    Just nodeAtPos@(label, _, _) = nodeAtPosM
    g' = colorNeighbors Blue nodeAtPos g

{- | If 'o' has been pressed all nodes will be colored orange
-}
eventloop ps@(ProgramState _ _ _ g) (InGraphs (Key ['o']))
  = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Colored all nodes orange\n"])
  where
    g' = colorAll Orange g

{- | If 'x' has been pressed, console will check whether strongly connected
-}
eventloop ps@(ProgramState _ _ _ g) (InGraphs (Key ['x']))
  = (ProgramState [] Nothing Nothing g, [OutGraphs $ DrawGraph g, OutStdOut $ S.StdOutMessage $ "Graph is strongly connected: " ++ (show b) ++ "\n"])
  where
    b = isStronglyConnected g


{- | If 'p' has been pressed, a node selected and a new node is selected
The console will output a boolean if there exists a path.
-}
eventloop ps@(ProgramState "p" (Just node1s) _ g) (InGraphs (Mouse (Click _) p))
  | nodeAtPosM == Nothing = (ps, [])
  | otherwise = (ProgramState [] Nothing Nothing g, [OutGraphs $ DrawGraph g, OutStdOut $ S.StdOutMessage $ "Path from node '" ++ [l1] ++ "' to '" ++ [l2] ++ "': " ++ (show b) ++ "\n"])
  where
    (l1, _, _) = node1s
    (l2, _, _) = nodeAtPos
    nodeAtPosM = onNode allNodes p
    allNodes = nodes g
    (Just nodeAtPos) = nodeAtPosM
    b = existsPath node1s nodeAtPos g

{- | If 'l' has been pressed, a node selected and a new node is selected
The console will output all possible paths (Without loops) from node to node
-}
eventloop ps@(ProgramState "l" (Just node1s) _ g) (InGraphs (Mouse (Click _) p))
  | nodeAtPosM == Nothing = (ps, [])
  | otherwise = (ProgramState [] Nothing Nothing g, [OutGraphs $ DrawGraph g, OutStdOut $ S.StdOutMessage $ "Paths from node '" ++ [l1] ++ "' to '" ++ [l2] ++ "': " ++ (show list) ++ "\n"])
  where
    (l1, _, _) = node1s
    (l2, _, _) = nodeAtPos
    nodeAtPosM = onNode allNodes p
    allNodes = nodes g
    (Just nodeAtPos) = nodeAtPosM
    list = prettyPathList $ paths g node1s nodeAtPos

{- | If 'n' has been pressed and the mouse has
clicked at a position where there is no node yet,
a new node is inserted at that point
-}
eventloop ps@(ProgramState "n" _ _ g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Inserted node '" ++ [nextlabel] ++ "'\n"])
    | otherwise             = (ps, [OutStdOut $ S.StdOutMessage "Tried to insert a node on another node"])
    where
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g
        nextlabel = nextLabel allNodes
        newNode = (nextlabel, p, Orange)
        g' = g {nodes=(newNode:allNodes)}



{- | Buffer the last node selected if it doesn't
  trigger an event on first spot
-}
eventloop ps@(ProgramState _ Nothing _ g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [])
    | otherwise             = (ps {node1Select = Just nodeAtPos}, [OutStdOut $ S.StdOutMessage $ "[1st Select] Click on node '" ++ [l] ++ "'\n"])
    where
        (l, _, _) = nodeAtPos
        (Just nodeAtPos) = nodeAtPosM
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g


{- | Buffer the last node selected if it doesn't trigger an event on second spot -}
eventloop ps@(ProgramState _ (Just _) Nothing g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [OutStdOut $ S.StdOutMessage "Clicked on not a node\n"])
    | otherwise             = (ps {node2Select = Just nodeAtPos}, [OutStdOut $ S.StdOutMessage $ "[2nd Select] Click on node '" ++ [l] ++ "'\n"])
    where
        (l, _, _) = nodeAtPos
        (Just nodeAtPos) = nodeAtPosM
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g


{- | Abort current operation and reset start on "esc" -}
eventloop ps (InGraphs (Key "esc"))
    = (ProgramState [] Nothing Nothing (graph ps), [OutStdOut $ S.StdOutMessage "Aborted current operation\n"])


{- | Stop the system on "s" -}
eventloop ps (InGraphs (Key "s"))
    = (ps, [OutStdOut $ S.StdOutMessage "Stopping system...\n", Stop])


{- | Buffer the last press key if it doesn't trigger an event -}
eventloop ps@(ProgramState _ _ _ _) (InGraphs (Key key))
    = (ps {pressedKey = key}, [OutStdOut $ S.StdOutMessage $ "Buffered keystroke '" ++ key ++ "'\n" ])


{- | For all other In events, do nothing -}
eventloop ps _ = (ps, [])
