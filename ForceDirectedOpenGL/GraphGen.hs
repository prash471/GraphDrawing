module GraphGen (
    list,
    GraphGen.cycle,
    star,
    complete,
) where
import qualified Graph (graphFromEdgeList, LabeledGraph)

list :: (Enum a, Num a, Show a) => a -> Graph.LabeledGraph
list n = Graph.graphFromEdgeList [(show v, show (v+1)) | v <- [1..(n-1)]]

cycle :: (Integral a, Show a) => a -> Graph.LabeledGraph
cycle n = Graph.graphFromEdgeList edges
    where edges = map (\(a, b) -> (show (a+1), show (b+1)))
                    [(v, (((v+1) `mod` n))) | v <- [0..n-1]]

star :: (Enum a, Num a, Show a) => a -> Graph.LabeledGraph
star n = Graph.graphFromEdgeList [("1", (show v)) | v <- [2..n]]

complete :: (Enum a, Eq a, Num a, Show a) => a -> Graph.LabeledGraph
complete n = Graph.graphFromEdgeList
    [((show a), (show b)) | a <- [1..n], b <- [1..n], a /= b]