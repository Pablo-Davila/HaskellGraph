
module Graph.Connection (
    connectedComponent,
    connectedComponents,
    stronglyConnectedComponent,
    stronglyConnectedComponents,
    isConnected,
    isStronglyConnected
) where

import Graph


-- Connection algorithms --

-- Obtain the connected component of a vertex in the graph
connectedComponent :: (Eq a, Eq b) => Graph a b -> Vertex a -> Graph a b
connectedComponent g v = connectedComponentAux g [v] ([v],[])

connectedComponentAux :: (Eq a, Eq b) => Graph a b -> [Vertex a] -> Graph a b -> Graph a b
connectedComponentAux _ [] acc = acc
connectedComponentAux g (v:vs) acc = connectedComponentAux g (vs++vs2) (vertices acc ++ vs2, edges acc ++ es)
    where
        es = filter (\e -> not (elem e (edges acc))) (vertexEdges g v)
        vs2 = filter
            (\v -> not (elem v (vertices acc)) && not (elem v vs))
            (neighbours g v)

-- Obtain all the connected components of the graph
connectedComponents :: (Eq a, Eq b) => Graph a b -> [Graph a b]
connectedComponents ([],[]) = []
connectedComponents g = g1:connectedComponents (g -* g1)
    where
        v = head $ vertices g
        g1 = connectedComponent g v

-- Obtain the strongly connected component of a vertex in the digraph
stronglyConnectedComponent :: (Eq a, Eq b) => Graph a b -> Vertex a -> Graph a b
stronglyConnectedComponent g v = stronglyConnectedComponentAux g [v] ([v],[])

stronglyConnectedComponentAux :: (Eq a, Eq b) => Graph a b -> [Vertex a] -> Graph a b -> Graph a b
stronglyConnectedComponentAux _ [] acc = acc
stronglyConnectedComponentAux g (v:vs) acc = stronglyConnectedComponentAux g (vs++vs2) (vertices acc ++ vs2, edges acc ++ es)
    where
        es = filter (\e -> not (elem e (edges acc))) (outEdges g v)
        vs2 = filter
            (\v -> not (elem v (vertices acc)) && not (elem v vs))
            [edgeTarget g e | e<-es]

stronglyConnectedComponents :: (Eq a, Eq b) => Graph a b -> [Graph a b]
stronglyConnectedComponents g = map (stronglyConnectedComponent g) (vertices g)

-- Check if a graph is connected
isConnected :: (Eq a, Eq b) => Graph a b -> Bool
isConnected g = length (vertices g) == length (vertices (connectedComponent g (head $ vertices g)))

-- Check if a digraph is strongly connected
isStronglyConnected :: (Eq a, Eq b) => Graph a b -> Bool
isStronglyConnected g = all aux (vertices g)
    where
        n = length (vertices g)
        aux v = n == length (vertices (stronglyConnectedComponent g v))
