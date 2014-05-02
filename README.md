GraphDrawing
============
To run *.lhs / *.hs files

ghc --make *.lhs

./* -o *.svg -w 300

Simple graph to create a graph having nodes on a regular hexagon (6 nodes).

Currently working with generalised spring layout algorithm.

A simple circle (circle.lhs)

![Build Status](http://projects.haskell.org/diagrams/doc/images/be89c2f6b4436ad7.png)

An edge (edge.lhs)

![Build Status](http://i58.tinypic.com/9r7kw6_th.jpg)

A simple graph

![Build Status](http://oi57.tinypic.com/149qrde.jpg)


SpringImplementation.hs file Includes Simple implementation of Spring force algorithm to draw graphs

![Build Status](http://oi62.tinypic.com/2hevzhx.jpg)

The above Image is output of 100 iterations of graph for fully connected graph

buildG (1,6)[(1,2),(1,3),(1,4),(1,5),(1,6),(2,3),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6),(4,5),(4,6),(5,6)]

where initial random position was around a parabola.

![Build Status](http://oi62.tinypic.com/530d5k.jpg)

The above Image is output of 100 iterations of graph with removing edge between (2,4)

buildG (1,6)[(1,2),(1,3),(1,4),(1,5),(1,6),(2,3),(2,5),(2,6),(3,4),(3,5),(3,6),(4,5),(4,6),(5,6)]

where initial random position was around a parabola.
