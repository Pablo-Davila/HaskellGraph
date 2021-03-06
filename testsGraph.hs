
import Data.Array

import Graph
import Graph.Samples
import Graph.Connection
import Graph.SpanningTree


testsGraph :: [Bool]
testsGraph = [  -- Graph tests
        g1 +* g2 -* g2 == g1,
        g1 +* g2 -* g1 == g2,

        findEdge g1 v1 v0 == Just (edge 0.5 0 1),
        findEdge g1 v0 v4 == Nothing,

        findEdgeDirected g1 v0 v1 == Just (edge 0.5 0 1),
        findEdgeDirected g1 v0 v4 == Nothing,

        adjacencyMatrixDirected g1 == array
            ((1,1),(5,5))
            [
                ((1,1),Nothing),((1,2),Just 0.5),((1,3),Nothing),((1,4),Nothing),((1,5),Nothing),
                ((2,1),Nothing),((2,2),Nothing),((2,3),Just 2.2),((2,4),Nothing),((2,5),Nothing),
                ((3,1),Nothing),((3,2),Nothing),((3,3),Nothing),((3,4),Just 1.3),((3,5),Just 3.2),
                ((4,1),Nothing),((4,2),Just 3.7),((4,3),Nothing),((4,4),Nothing),((4,5),Nothing),
                ((5,1),Nothing),((5,2),Nothing),((5,3),Nothing),((5,4),Nothing),((5,5),Nothing)
            ],
        adjacencyMatrix g1 == array
            ((1,1),(5,5))
            [
                ((1,1),Just 0.5),((1,2),Just 0.5),((1,3),Nothing),((1,4),Nothing),((1,5),Nothing),
                ((2,1),Just 0.5),((2,2),Just 0.5),((2,3),Just 2.2),((2,4),Just 3.7),((2,5),Nothing),
                ((3,1),Nothing),((3,2),Just 2.2),((3,3),Just 2.2),((3,4),Just 1.3),((3,5),Just 3.2),
                ((4,1),Nothing),((4,2),Just 3.7),((4,3),Just 1.3),((4,4),Just 1.3),((4,5),Nothing),
                ((5,1),Nothing),((5,2),Nothing),((5,3),Just 3.2),((5,4),Nothing),((5,5),Just 3.2)
            ]
    ]

testsSamples :: [Bool]
testsSamples = [  -- Graph.Samples tests
        checkGraphIds g1,
        checkGraphIds g2,
        checkGraphIds g3,
        checkGraphIds g4
    ]

testsConnection :: [Bool]
testsConnection = [  -- Graph.Connection tests
        connectedComponent g1 v0 == graph
            [(0,"Vertex 0"),(1,"Vertex 1"),(3,"Vertex 3"),(2,"Vertex 2"),(4,"Vertex 4")]
            [(0.5,0,1),(3.7,3,1),(2.2,1,2),(1.3,2,3),(3.2,2,4)],
        connectedComponent g3 v5 == graph
            [(5,"Vertex 5"),(6,"Vertex 6"),(8,"Vertex 8"),(7,"Vertex 7"),(9,"Vertex 9")]
            [(0.5,5,6),(1.5,5,8),(2.2,7,6),(3.7,8,6),(1.3,7,8),(3.2,7,9)],

        connectedComponents g3 == [
                graph
                    [(5,"Vertex 5"),(6,"Vertex 6"),(8,"Vertex 8"),(7,"Vertex 7"),(9,"Vertex 9")]
                    [(0.5,5,6),(1.5,5,8),(2.2,7,6),(3.7,8,6),(1.3,7,8),(3.2,7,9)],
                graph
                    [(0,"Vertex 0"),(1,"Vertex 1"),(3,"Vertex 3"),(2,"Vertex 2"),(4,"Vertex 4")]
                    [(0.5,0,1),(3.7,3,1),(2.2,1,2),(1.3,2,3),(3.2,2,4)]
            ],

        stronglyConnectedComponent g1 v0 == graph
            [(0,"Vertex 0"),(1,"Vertex 1"),(2,"Vertex 2"),(3,"Vertex 3"),(4,"Vertex 4")]
            [(0.5,0,1),(2.2,1,2),(1.3,2,3),(3.2,2,4),(3.7,3,1)],
        stronglyConnectedComponent g1 v1 == graph 
            [(1,"Vertex 1"),(2,"Vertex 2"),(3,"Vertex 3"),(4,"Vertex 4")]
            [(2.2,1,2),(1.3,2,3),(3.2,2,4),(3.7,3,1)],

        stronglyConnectedComponents g2 == [
            graph [(5,"Vertex 5"),(6,"Vertex 6"),(8,"Vertex 8")] [(0.5,5,6),(1.5,5,8),(3.7,8,6)],
            graph [(6,"Vertex 6")] [],
            graph [(7,"Vertex 7"),(6,"Vertex 6"),(8,"Vertex 8"),(9,"Vertex 9")] [(2.2,7,6),(1.3,7,8),(3.2,7,9),(3.7,8,6)],
            graph [(8,"Vertex 8"),(6,"Vertex 6")] [(3.7,8,6)],
            graph [(9,"Vertex 9")] []
        ],

        isConnected g1 == True,
        isConnected g3 == False,

        isStronglyConnected g1 == False,
        isStronglyConnected g4 == True
    ]

testsSpanningTree :: [Bool]
testsSpanningTree = [  -- Graph.SpanningTree tests
        spanningTree g1 == graph
            [(0,"Vertex 0"),(1,"Vertex 1"),(3,"Vertex 3"),(2,"Vertex 2"),(4,"Vertex 4")]
            [(0.5,0,1),(3.7,3,1),(2.2,1,2),(3.2,2,4)],
        spanningTree g2 == graph
            [(5,"Vertex 5"),(6,"Vertex 6"),(8,"Vertex 8"),(7,"Vertex 7"),(9,"Vertex 9")]
            [(0.5,5,6),(1.5,5,8),(2.2,7,6),(3.2,7,9)],

        minSpanningTree g2 == graph
            [(5,"Vertex 5"),(6,"Vertex 6"),(7,"Vertex 7"),(8,"Vertex 8"),(9,"Vertex 9")]
            [(0.5,5,6),(1.3,7,8),(1.5,5,8),(3.2,7,9)],
        minSpanningTree g4 == graph
            [(0,"Vertex 0"),(1,"Vertex 1"),(2,"Vertex 2")]
            [(0.5,0,1),(1.3,2,0)],
        
        minSpanningForest g3 == [
            graph
                [(5,"Vertex 5"),(6,"Vertex 6"),(8,"Vertex 8"),(7,"Vertex 7"),(9,"Vertex 9")]
                [(0.5,5,6),(1.3,7,8),(1.5,5,8),(3.2,7,9)],
            graph
                [(0,"Vertex 0"),(1,"Vertex 1"),(3,"Vertex 3"),(2,"Vertex 2"),(4,"Vertex 4")]
                [(0.5,0,1),(1.3,2,3),(2.2,1,2),(3.2,2,4)]
        ]
    ]


result :: IO()
result
    | and tests = putStrLn "Everything correct!"
    | otherwise = putStrLn $ show nFailed ++ " failed test" ++ if nFailed==1 then "!" else "s!"
    where
        tests = concat [testsGraph, testsSamples, testsConnection, testsSpanningTree]
        nFailed = length [b | b<-tests, b==False]
