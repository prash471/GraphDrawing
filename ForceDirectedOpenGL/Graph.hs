module Graph (
    graphFromEdgeList,
    graphFromMap,
    LabeledGraph,
    ) where

import qualified Data.Graph as Graph
import System.IO
import Data.List.Split (splitOn, endBy)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Data.Set as Set

type LabeledGraph =
    ( Graph.Graph 
    , Graph.Vertex -> (String, String, [String])
    , String -> Maybe Graph.Vertex )

graphFromEdgeList :: [(String, String)] -> LabeledGraph
graphFromEdgeList edges = graphFromMap (Map.union map1 map2)
  where 
    edge_list = map (\(a, b) -> (a, [b])) edges
    map1 = Map.fromListWith (++) edge_list
    orphans = Set.difference (Set.fromList (map snd edges)) 
        (Set.fromList (map fst edges))
    orphan_edges = map (\x -> (x, [])) (Set.toList orphans)
    map2 = Map.fromList orphan_edges

graphFromMap m = Graph.graphFromEdges (map (\(k,v) -> (k,k,v)) (Map.toList m))