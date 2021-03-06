module Data.SpringAlgo where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Array
import Data.List
import GHC.Float
data Point2 n = Point2 n n

--graph1 = buildG (1, 6) [(1, 2), (1, 3), (1,5),(2, 4), (5, 6)]
--graph1 = buildG (1,6)[(1,2),(2,3),(3,4),(4,5),(5,6),(1,6)]


reverseE    :: Graph -> [Edge]
reverseE g   = [ (w, v) | (v, w) <- edges g ]

undirected   :: Graph -> Graph
undirected g  = buildG (bounds g) (edges g ++ reverseE g)

graph =  undirected graph1

--___________________________________________________________________________________________________________________________

node :: Int -> Diagram B R2
node n = text (show n) # scale 0.2 # fc white
       <> circle 0.2 # fc green # named n

arrowOpts = with & headGap .~ 0.07
                  & tailGap .~ 0.07
                  & headSize .~ 0.2

tournament :: Int -> Diagram B R2
tournament n = decorateTrail (regPoly n 1) (map node [1..n])
   # applyAll [connectOutside' arrowOpts j k | j <- [1 , 3 , 5], k <- [j+1 .. n]]
--example = tournament 6

example = position (zip (map mkPoint listPoint) (nodes)) --[connectPerim (mkPoint (listPoint!!j)) (mkPoint (listPoint!!k)) | j <- [1 , 3 , 5], k <- [j+1 .. 6]]
	where 
	nodes     = map node [1..6]
	listPoint = arrayToList 6 (getUpdatedArrays graph 100)
	mkPoint (x1,y1) = p2 (float2Double x1,float2Double y1)

--main = mainWith (example :: Diagram B R2)

--___________________________________________________________________________________________________________________________

verticesCount :: Graph -> Int
verticesCount g = length $ vertices g

--___________________________________________________________________________________________________________________________

listNodes :: Graph -> Int -> [Int]
listNodes g v = g!v

--___________________________________________________________________________________________________________________________

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

--___________________________________________________________________________________________________________________________

assignRandomLoc :: Int -> Array Int (Float,Float)
assignRandomLoc n  =  a  
	where 
	a = array (1,n)([(1, (intToFloat 1,intToFloat (n*n)))] ++ [(i, (intToFloat (i*i),intToFloat ((n-(i+1))*((n-(i+1))))) )| i <- [2..n]])

--___________________________________________________________________________________________________________________________

arrayToList :: Int -> Array Int (Float,Float) -> [(Float,Float)]
arrayToList n arr  =  toList  
	where 
	toList = [arr!1] ++ [arr!i | i <- [2..n]]

--___________________________________________________________________________________________________________________________
	
distance2D :: Floating n => Point2 n -> Point2 n -> n
distance2D (Point2 x1 y1) (Point2 x2 y2) = sqrt (x'*x' + y'*y')
	where
		x' = x1 - x2
		y' = y1 - y2
--___________________________________________________________________________________________________________________________

forceTotalAttractive :: Floating n => Point2 n -> Point2 n -> (n,n)
forceTotalAttractive (Point2 x1 y1) (Point2 x2 y2) = showPoint (Point2 x' y')
	where
		r = distance2D (Point2 x1 y1) (Point2 x2 y2)
		x' = (x2 - x1)*2*(log r)/r
		y' = (y2 - y1)*2*(log r)/r

--___________________________________________________________________________________________________________________________

forceTotalRepulsive :: Floating n => Point2 n -> Point2 n -> (n,n)
forceTotalRepulsive (Point2 x1 y1) (Point2 x2 y2) = showPoint (Point2 x' y')
	where
		r = distance2D (Point2 x1 y1) (Point2 x2 y2)
		x' = (x1 - x2)/sqrt r
		y' = (y1 - y2)/sqrt r

--___________________________________________________________________________________________________________________________
		
showPoint :: Floating n => Point2 n -> (n,n)
showPoint (Point2 x y) = (x,y)

--___________________________________________________________________________________________________________________________

arrayInitialize :: Int -> [Int]
arrayInitialize n =	1:concat[[x] | x <- [2..n]]


--___________________________________________________________________________________________________________________________

getRowArray :: Int -> Int -> Array (Int , Int) (Float,Float) -> Array Int (Float,Float)
getRowArray rowNum totalColumn fromArray = toArray
	where
		toArray = array (1,totalColumn)([(1, fromArray!(rowNum ,1))] ++ [(i, fromArray!(rowNum ,i))| i <- [2..totalColumn]])

--___________________________________________________________________________________________________________________________

--getUpdatedArrays :: Graph -> Int -> Array (Int , Int) (Float,Float)
getUpdatedArrays :: Graph -> Int -> Array Int (Float,Float)
getUpdatedArrays g numItr= arrayReturnSingle
	where
		graphCount = verticesCount g
		randomArray = assignRandomLoc graphCount
		n = graphCount
		m = numItr
		arrayReturn = array ((1,1),(m,n))([((1,1), (updateArray g randomArray)!1)] ++ [((1,j), (updateArray g randomArray)!j)| j <- [2..n]] ++ [((i,j), (updateArray g (getRowArray (i-1) graphCount arrayReturn))!j)| i<-[2..m],j <- [1..n]] )
		arrayReturnSingle = getRowArray m graphCount arrayReturn
--___________________________________________________________________________________________________________________________

updateArray :: Graph -> (Array Int (Float,Float)) -> (Array Int (Float,Float))
updateArray g randomArray = (outLoc1)
	where
		graphCount = verticesCount g
		-- randomArray = assignRandomLoc graphCount
		forceArrayTemp = calculateTotalForce g randomArray
		-- verticesAll = ($ vertices g)
		-- calculatedForce = calculateArrayForce verticesAll randomArray g
		-- outLoc2 = randomArray
		n = graphCount		
		-- verticesAll = arrayInitialize n
		outLoc1 = array (1,n)([(1, sumTuples (randomArray!1) (forceArrayTemp!1))] ++ [(i, sumTuples (randomArray!i) (forceArrayTemp!i))| i <- [2..n]])
		-- outLoc1 = calculateArrayForce 1 verticesAll randomArray g

--___________________________________________________________________________________________________________________________

sumTuples :: (Float , Float) -> (Float , Float) -> (Float , Float)
sumTuples tup1 tup2 = (x , y)
	where
		(x1,y1) = tup1
		(x2,y2) = tup2
		(x,y) = (x1+(0.8*x2),y1+(0.8*y2))
--___________________________________________________________________________________________________________________________

calculateTotalForce :: Graph -> ( Array Int (Float,Float)) -> ( Array Int (Float,Float))
calculateTotalForce g randomArray = (outLoc1)
	where
		graphCount = verticesCount g
		-- randomArray = assignRandomLoc graphCount
		-- verticesAll = ($ vertices g)
		-- calculatedForce = calculateArrayForce verticesAll randomArray g
		-- outLoc2 = randomArray
		n = graphCount		
		verticesAll = arrayInitialize n
		outLoc1 = array (1,n)([(1, calculateArrayForce 1 verticesAll randomArray g)] ++ [(i, calculateArrayForce i verticesAll randomArray g) | i <- [2..n]])
		-- outLoc1 = calculateArrayForce 1 verticesAll randomArray g
--___________________________________________________________________________________________________________________________

calculateArrayForce :: Int -> [Int] -> Array Int (Float, Float) -> Graph -> (Float, Float)
calculateArrayForce self verticesAll randomArray g = (x3,y3)
	where
		neighbourList = listNodes g self
		nonNeighbourList = (verticesAll \\ neighbourList) \\ [self]
		x1 = calculateNeighbourForceX self neighbourList randomArray
		y1 = calculateNeighbourForceY self neighbourList randomArray
		x2 = calculateNonNeighbourForceX self nonNeighbourList randomArray
		y2 = calculateNonNeighbourForceY self nonNeighbourList randomArray
		x3 = x1+x2
		y3 = y1+y2
		-- arrayFinal = (x1+x2,y1+y2)

--___________________________________________________________________________________________________________________________

calculateNeighbourForceX :: Int -> [Int] -> Array Int (Float,Float) -> Float
calculateNeighbourForceX self [] randomArray = 0.0
calculateNeighbourForceX self [x] randomArray = pointForceX
	where
		(x1,y1) = randomArray!self
		(x2,y2) = randomArray!x
		(x3,y3) = forceTotalAttractive (Point2 x1 y1) (Point2 x2 y2)
		pointForceX = x3
calculateNeighbourForceX self neighbours randomArray = pointForceX
	where
		headNeighbour = head neighbours
		tailNeighbours = tail neighbours
		(x1,y1) = randomArray!self
		(x2,y2) = randomArray!headNeighbour
		(x3,y3) = forceTotalAttractive (Point2 x1 y1) (Point2 x2 y2)
		pointForceX = x3 + calculateNeighbourForceX self tailNeighbours randomArray

--___________________________________________________________________________________________________________________________

calculateNeighbourForceY :: Int -> [Int] -> Array Int (Float,Float) -> Float
calculateNeighbourForceY self [] randomArray = 0.0
calculateNeighbourForceY self [y] randomArray = pointForceY
	where
		(x1,y1) = randomArray!self
		(x2,y2) = randomArray!y
		(x3,y3) = forceTotalAttractive (Point2 x1 y1) (Point2 x2 y2)
		pointForceY = y3
calculateNeighbourForceY self neighbours randomArray = pointForceY
	where
		headNeighbour = head neighbours
		tailNeighbours = tail neighbours
		(x1,y1) = randomArray!self
		(x2,y2) = randomArray!headNeighbour
		(x3,y3) = forceTotalAttractive (Point2 x1 y1) (Point2 x2 y2)
		pointForceY = y3 + calculateNeighbourForceY self tailNeighbours randomArray

--___________________________________________________________________________________________________________________________

calculateNonNeighbourForceX :: Int -> [Int] -> Array Int (Float,Float) -> Float
calculateNonNeighbourForceX self [] randomArray = 0.0
calculateNonNeighbourForceX self [x] randomArray = pointForceX
	where
		(x1,y1) = randomArray!self
		(x2,y2) = randomArray!x
		(x3,y3) = forceTotalRepulsive (Point2 x1 y1) (Point2 x2 y2)
		pointForceX = x3
calculateNonNeighbourForceX self neighbours randomArray = pointForceX
	where
		headNeighbour = head neighbours
		tailNeighbours = tail neighbours
		(x1,y1) = randomArray!self
		(x2,y2) = randomArray!headNeighbour
		(x3,y3) = forceTotalRepulsive (Point2 x1 y1) (Point2 x2 y2)
		pointForceX = x3 + calculateNonNeighbourForceX self tailNeighbours randomArray

--___________________________________________________________________________________________________________________________

calculateNonNeighbourForceY :: Int -> [Int] -> Array Int (Float,Float) -> Float
calculateNonNeighbourForceY self [] randomArray = 0.0
calculateNonNeighbourForceY self [y] randomArray = pointForceY
	where
		(x1,y1) = randomArray!self
		(x2,y2) = randomArray!y
		(x3,y3) = forceTotalAttractive (Point2 x1 y1) (Point2 x2 y2)
		pointForceY = y3
calculateNonNeighbourForceY self neighbours randomArray = pointForceY
	where
		headNeighbour = head neighbours
		tailNeighbours = tail neighbours
		(x1,y1) = randomArray!self
		(x2,y2) = randomArray!headNeighbour
		(x3,y3) = forceTotalRepulsive (Point2 x1 y1) (Point2 x2 y2)
		pointForceY = y3 + calculateNonNeighbourForceY self tailNeighbours randomArray

