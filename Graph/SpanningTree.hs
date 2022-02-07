
module Graph.SpanningTree (
    spanningTree,
    spanningForest,
    minSpanningTree,
    minSpanningForest
) where

import Data.List (sortBy)
import Graph
import Graph.Connection


-- Tree cover algorithms --

-- Get a spanning tree of a connected graph
spanningTree :: (Eq a, Eq b) => Graph a b -> Graph a b
spanningTree g
    | isConnected g = aux (vertices g) ([head (vertices g)],[])
    | otherwise = error "The graph must be connected"
    where
        aux [] acc = acc 
        aux (v:vs) (vacc,eacc) = aux vs (vacc++vIn++vOut, eacc++eIn++eOut)
            where
                eIn = [e | e<-inEdges g v, not (elem (edgeSource g e) vacc)]
                vIn = [edgeSource g e | e<-eIn]
                eOut = [e | e<-outEdges g v, not (elem (edgeTarget g e) (vacc++vIn))]
                vOut = [edgeTarget g e | e<-eOut]

-- Get a spanning forest a graph
spanningForest :: (Eq a, Eq b, Ord b) => Graph a b -> [Graph a b]
spanningForest g = [spanningTree h | h<-connectedComponents g]


-- Minimum spanning tree algorithms --

-- Get a minimum spanning tree of a connected graph
minSpanningTree :: (Eq a, Eq b, Ord b) => Graph a b -> Graph a b
minSpanningTree g
    | isConnected g = graph (vertices g) treeEdges
    | otherwise = error "The graph must be connected"
    where
        ids = map vertexId (vertices g)
        treeEdges = aux (zip ids ids) (customSortBy edgeTag (edges g))
        aux :: [(Int,Int)] -> [Edge b] -> [Edge b]
        aux vs [] = []
        aux vs (e:es)
            | vertexTag v1 == vertexTag v2 = aux vs es
            | otherwise = e:aux vsNew es
            where
                v1 = edgeSource (vs,[e]) e
                v2 = edgeTarget (vs,[e]) e
                vsNew = [(id, if cc==vertexTag v2 then vertexTag v1 else cc) | (id,cc)<-vs]

-- Get a minimum spanning forest of a graph
minSpanningForest :: (Eq a, Eq b, Ord b) => Graph a b -> [Graph a b]
minSpanningForest g = [minSpanningTree h | h<-connectedComponents g]


-- Utils --

customSortBy :: (Ord o) => (a -> o) -> [a] -> [a]
customSortBy f xs = sortBy (\x y -> compare (f x) (f y)) xs
