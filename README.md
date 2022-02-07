
# HaskellGraph

HaskellGraph is a Graph library for Haskell. It was initially developed as part of an assignment for the subject "Delarative Programming" at the University of Seville.

The remaining part is [HaskellStateSpaceSearch](https://github.com/Pablo-Davila/HaskellStateSpaceSearch), a library used to solve [state space search](https://en.wikipedia.org/w/index.php?title=State_space_search&oldid=1051787592) problems.


## Contents

 - `Graph`: Implementation of the weighted graph/digraph data structure along with a set of helper functions.
 - `Graph.Samples`: A collection of sample graphs and functions to draw them using the interactive interpreter.
 - `Graph.Connection`: This submodule contains tools to work with (strongly)connected components and check the connectivity of a (di)graph.
 - `Graph.SpanningTree`: This submodule contains tools to obtain spanning trees and forests as well as minimum spanning trees and forests.

_Note_: [testsGraph.hs](./testsGraph.hs) contains usage examples of all the modules.


## Usage

You may import the modules you need by using the following lines:

``` Haskell
import Graph
import Graph.Samples
import Graph.Connection
import Graph.TreeCover
```
