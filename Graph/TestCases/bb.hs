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


makeDotPng :: String -> String -> IO ()
makeDotPng name src = do
  let dotfile = name ++ ".dot"
      pngfile = name ++ ".png"
  writeFile dotfile src
  system $ unwords ["dot", dotfile, "-Tpng", ">", pngfile]
  return ()

main = do
  makeDotPng "00" $ graphviz' myGraph
  makeDotPng "01" $ graphviz myGraph "test" (0, 0) (0, 0) Landscape
  makeDotPng "02" $ graphviz myGraph "test" (0, 0) (0, 0) Portrait
  print "------"
