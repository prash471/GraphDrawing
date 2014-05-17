module Main where
 
import System.Cmd
import Data.Graph.Inductive
import Data.Graph.Inductive.Graphviz
import Data.Graph.Inductive.Example
 
myGraph :: Gr String ()
--myGraph = buildG (1,6)[(1,2),(1,3),(1,4),(1,5),(1,6),(2,3),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6),(4,5),(4,6),(5,6)]
myGraph = mkGraph
          [(1, "A"), (2, "B"), (3, "C"), (4, "D"), (5, "E"), (6, "F")]
          [(1,2, ()),(2,3, ()),(3,4, ()),(4,5, ()),(5,6, ()),(6,1, ())]
--	  [(2, 1, ()), (2, 3, ()), (4, 2, ()), (4, 5, ()), (0, 6, ())]
--graph1 = myGraph 

makeDotPng :: String -> String -> IO ()
makeDotPng name src = do
  let dotfile = name ++ ".dot"
      pngfile = name ++ ".png"
  writeFile dotfile src
  system $ unwords ["dot", dotfile, "-Tpng", ">", pngfile]
  return ()

--main = mainWith (example :: Diagram B R2)

main = do
  makeDotPng "00" $ graphviz' myGraph
  makeDotPng "01" $ graphviz myGraph "test" (0, 0) (0, 0) Landscape
  makeDotPng "02" $ graphviz myGraph "test" (0, 0) (0, 0) Portrait
  print "------"
{-  let dg name graph = makeDotPng name $ graphviz graph name (0,0) (0,0) Portrait
  dg "a" a
  dg "dag3" dag3
  dg "abb" abb
  dg "ab" ab
  dg "loop" loop
  dg "e" e
  dg "c" c
  dg "b" b
  dg "e3" e3
  dg "cyc3" cyc3
  dg "g3b" g3b
  dg "g3" g3
  dg "dag4" dag4
  dg "d1" d1
  dg "d3" d3
  dg "clr479" clr479
  dg "clr489" clr489
  dg "clr486" clr486
  dg "clr508" clr508
  dg "clr528" clr528
  dg "clr595" clr595
  dg "gr1" gr1
  dg "kin248" kin248
  dg "vor" vor
  print "------"
-}
