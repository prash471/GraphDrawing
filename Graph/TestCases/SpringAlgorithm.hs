import Data.SpringAlgo
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Graph

graph1 = buildG (1,6)[(1,2),(1,3),(1,4),(1,5),(1,6),(2,3),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6),(4,5),(4,6),(5,6)]
main = mainWith (example :: Diagram B R2)
