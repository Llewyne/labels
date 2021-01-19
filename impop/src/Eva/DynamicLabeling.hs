{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Eva.DynamicLabeling where

import Data.List hiding (intersect)
import Data.List.Split
import Debug.Trace
import Data.Ord
import Data.Vinyl.CoRec
import Data.Ext

import Data.Array((!),array)

import Tests


import Control.Lens

import Data.Geometry hiding (head,direction,init,replicate,unit)
import Data.Geometry.PlanarSubdivision hiding(location)
--import Data.Geometry.Ipe

import CurveArrangement
import CurveArrangement.Types

import Nonogram

import           Data.Geometry.Polygon

_line :: Port -> Line 2 Float
_line p = Data.Geometry.Line (_location p) (Nonogram._direction p)

--Test
pDummy1 = Port (Point2 0 0) (Vector2 1 1) True
p1 = Port (Point2 0 2) (Vector2 (-1) 1) True
p2 = Port (Point2 0 4) (Vector2 1 1) False
p3 = Port (Point2 0 7) (Vector2 (-1) 1) True
p4 = Port (Point2 0 9) (Vector2 1 1) False
pDummy2 = Port (Point2 0 12) (Vector2 1 1) False

-- test1 = [([p1],[1,2]::[Int]),([p2],[2,3]::[Int]),([p3],[1]::[Int]),([p4],[1]::[Int]),([p5],[1]::[Int])]

width = 600

height = 600

boxSize = 1

boxPath = [(0,0),(boxSize,0),(boxSize,boxSize),(0,boxSize)]

hw = width / 2

hh = height / 2

parseLabel :: String -> UnplacedLabel
parseLabel s = ([(Port (Point2 0 (height-y)) (parseVector angle) (side /= 1))], nums)
    where 
        ws = words s
        y = read (ws!!0)
        angle = read (ws!!1)
        side = read (ws!!3)
        nums = map read (splitOn "," (init (tail (ws!!2))))

parseVector 0   = Vector2 0 0 
parseVector (-45)  = Vector2 1 1
parseVector (45) = Vector2 1 (-1)
parseVector _ = Vector2 0 0 

--nonogramToPicture :: Nonogram -> Picture
--nonogramToPicture n = Drawing.nonogram 

-- labelsToPicture :: [Label] -> Picture
-- labelsToPicture ls = Pictures (map labelToPicture ls)


-- labelToPicture :: Label -> Picture
-- labelToPicture (Label c (Port (Point2 x y) (Vector2 vx vy) s) o)  = Pictures (leader (x,y) c o (vx/vy) (fromEnum s))

type FSUnplacedLabel = (Port, Clue) --Unplaced Label with a fixed side

unit = 3

frame_ :: SimplePolygon () Float
frame_ = fromPoints [Point2 (112.0) (-128.0) :+ (),Point2 (128.0) (-128.0) :+ (),Point2 (128.0) (-96.0) :+ (),Point2 (128.0) (128.0) :+ (),Point2 (16.0) (128.0) :+ (),Point2 (-16.0) (128.0) :+ (),Point2 (-128.0) (128.0) :+ (),Point2 (-128.0) (-48.0) :+ (),Point2 (-128.0) (-96.0) :+ (),Point2 (-128.0) (-128.0) :+ ()]

uls :: [UnplacedLabel]
uls = [([Port {_location = Point2 (112.0) (-128.0), Nonogram._direction = Vector2 (0.44721276) (-0.89442766), _side = True},Port {_location = Point2 (-16.0) (128.0), Nonogram._direction = Vector2 (-0.44721353) (0.89442724), _side = False}],[1]),
    ([Port {_location = Point2 (128.0) (-96.0), Nonogram._direction = Vector2 (0.98287207) (-0.18428911), _side = True},Port {_location = Point2 (-128.0) (-48.0), Nonogram._direction = Vector2 (-0.98287266) (0.18428588), _side = False}],[0]),
    ([Port {_location = Point2 (16.0) (128.0), Nonogram._direction = Vector2 (0.5407575) (0.84117854), _side = True},Port {_location = Point2 (-128.0) (-96.0), Nonogram._direction = Vector2 (-0.5407594) (-0.8411773), _side = False}],[1]),
    ([Port {_location = Point2 (-16.0) (128.0), Nonogram._direction = Vector2 (-0.44721353) (0.89442724), _side = True},Port {_location = Point2 (112.0) (-128.0), Nonogram._direction = Vector2 (0.44721276) (-0.89442766), _side = False}],[0]),
    ([Port {_location = Point2 (-128.0) (-48.0), Nonogram._direction = Vector2 (-0.98287266) (0.18428588), _side = True},Port {_location = Point2 (128.0) (-96.0), Nonogram._direction = Vector2 (0.98287207) (-0.18428911), _side = False}],[1]),
    ([Port {_location = Point2 (-128.0) (-96.0), Nonogram._direction = Vector2 (-0.5407594) (-0.8411773), _side = True},Port {_location = Point2 (16.0) (128.0), Nonogram._direction = Vector2 (0.5407575) (0.84117854), _side = False}],[0])]

-- places labels dynamically
placeLabelsDynamic :: [UnplacedLabel] -> Frame -> [Label] 
placeLabelsDynamic ls f = concat $ map (placeLabelsDynamicEdge ls_) (simplify $ listEdges f)
    where ls_ = assignPorts ls

simplify :: [LineSegment 2 () Float] -> [LineSegment 2 () Float]
simplify (l:ll:ls)
    | abs (getM l - getM ll) < 0.01 || getM l == getM ll = simplify (((l&start .~ l^.start)&end .~ ll^.end) : ls)
    | otherwise = l:(simplify (ll:ls))
simplify [l] = [l]

getM :: LineSegment 2 () Float -> Float
getM l = (l^.end.core.xCoord - l^.start.core.xCoord) / (l^.end.core.yCoord - l^.start.core.yCoord)

-- -- -- places labels dynamically on an edge
placeLabelsDynamicEdge :: [FSUnplacedLabel] -> LineSegment 2 () Float -> [Label]
placeLabelsDynamicEdge ls s = zipWith placeLabel edgeLabels (placeLabelsDynamicEdge_ ls_ 1 (length edgeLabels) 1 1)
    where
        ls_ = dummy0 s_ : makeTopEdge edgeLabels s ++ [dummyNplus1 s_] -- Rotated labels with added dummies
        s_ = pivotLineSegment s
        edgeLabels = getEdgeLabels ls s

placeLabel :: FSUnplacedLabel -> Int -> Label
placeLabel ul e = Label (snd ul) (fst ul) (fromIntegral e)

-- placeLabelsDynamicEdge_ :: Int -- Index of the first label 
--     -> Int -- Index of the last label
--     -> [Float] -- Leader lengths (0 if unassigned)
--     -> [FSUnplacedLabel] -- Unplaced labels with fixed port assignment
--     -> LineSegment 2 () Float -- Edge to be labeled
--     -> [Label] -- Placed labels
-- placeLabelsDynamicEdge_ i1 iN leaders labels s = 
    
minExtLength ls p1 p2 e1 e2 = r!(p1,p2)
    where
            r = array((p1,p1),(p2,p2)) [((i,j),(f i j e1 e2))|i<-[p1..p2],j<-[p1..p2]]
            eMax = (p2-p1)^2
            f i j a b 
                | i == j - 1 = a + b
                | j - 1 > 1 = minimum [(f i k a c) + (f k j c b) - c | k<-[i+1..j-1], c<- [1..max 1 (min a b)], elem c (set k), valid i a j b k c]
            set k = [0..(p2-p1)^2]
            valid i a j b k c = fitLength (ls!!i) a (ls!!j) b (ls!!k) c

-- placeLabelsDynamicEdge_ :: [Port] -> Int -> Int -> Int -> Int -> Array (Int, Int) (Int, (Int, Int))
placeLabelsDynamicEdge_ ls p1 p2 e1 e2 = e1:getLengths r p1 p2++[e2]
    where
            r = array((p1,p1),(p2,p2)) [((i,j),(f i j e1 e2))|i<-[p1..p2],j<-[p1..p2]]
            eMax = (p2-p1)^2
            f i j a b 
                | i == j - 1 = (a + b,(0,0))
                | j - 1 > 1 = minimum [(fst (f i k a c) + fst (f k j c b) - c,(k,c)) | k<-[i+1..j-1], c<- [1..max 1 (min a b)], elem c (set k), valid i a j b k c]
            set k = [0..(p2-p1)^2]
            valid i a j b k c = fitLength (fst (ls!!i)) a (fst (ls!!j)) b (fst (ls!!k)) c

-- test_ = placeLabelsDynamicEdge_ [pDummy1,p1,p2,p3,p4,pDummy2] 2 3 1 1

-- getLengths :: (Ix t, Eq a1, Num t, Num a1) => Array (t, t) (a2, (t, a1)) -> t -> t -> [a1]
getLengths r p1 p2
    | p1 == p2 || snd l == (0,0) = []
    | otherwise = getLengths r p1 (fst (snd l)) ++ [ snd (snd l)] ++ getLengths r (fst (snd l)) p2
    where l = r!(p1,p2)
 

-- determines if a leader with length len fits between two ports with lengths len1 and len2
-- fitLength :: Port -> Integer -> Port -> Integer -> Port -> Integer -> Bool
fitLength (Port pos1 dir1 s1) len1 (Port pos2 dir2 s2) len2 (Port pos dir s) len 
    | s1 /= s2 = True
    | leaderIntersect (pos1^.xCoord) m1 (pos^.xCoord) m (fromIntegral (len1 + boxSize),fromIntegral (len + boxSize)) = False
    | leaderIntersect (pos^.xCoord) m (pos2^.xCoord) m2 (fromIntegral (len + boxSize),fromIntegral (len2 + boxSize)) = False
    | otherwise = True
    where
        m = (dir^.xComponent) / (dir^.yComponent)
        m1 = (dir1^.xComponent) / (dir1^.yComponent)
        m2 = (dir2^.xComponent) / (dir2^.yComponent)

-- Do two leaders, defined by their y-coordinate position, slope and length, intersect
leaderIntersect :: Float -> Float -> Float-> Float -> (Integer,Integer) -> Bool
leaderIntersect y1 m1 y2 m2 (len1,len2) 
    | m1 == m2 || px <= 0 = False
    | ((px < ((sin (atan m1))*(fromIntegral len1))) && (px < abs ((sin (atan m2))*(fromIntegral len2)))) = True
    | otherwise = False
    where (px,py) = lineIntersection m1 y1 m2 y2

-- intersection point = ((y2 - y1)/(m1-m2),(m1*y2-m2*y1)/(m1-m2)
lineIntersection :: Float -> Float -> Float -> Float -> (Float,Float)
lineIntersection m1 b1 m2 b2 = ((b2-b1)/(m1-m2),(m1*b1-m2*b2)/(m1-m2))

-- Assigns ports to unplaced labels 
assignPorts :: [UnplacedLabel] -> [FSUnplacedLabel]
assignPorts [] = []
assignPorts ((ps,c):ls) =  (ps!!0, c):assignPorts_ ls
assignPorts_ [] = []
assignPorts_ ((ps,c):ls) 
    | length ps > 1 = (ps!!1, c):assignPorts ls
    | otherwise = (ps!!0, c):assignPorts ls

-- initialize leader lengths
initLeaders l = (maxLength:leader1:take (l-2) (repeat 0))++ [leaderN,maxLength]
    where maxLength = l^2 -- maxLength of leader

-- Makes dummies, assumes line segment is horizontal
dummy0 :: LineSegment 2 () Float -> FSUnplacedLabel
dummy0 s = (Port (Point2 (((s^.start.core.xCoord)) - unit) ((s^.start.core.yCoord))) (Vector2 (-unit) unit) False,[0])
dummyNplus1 :: LineSegment 2 () Float -> FSUnplacedLabel
dummyNplus1 s = (Port (Point2 ((s^.end.core.xCoord) - unit) (s^.end.core.yCoord)) (Vector2 unit unit) True,[0])

-- Sets start and end leader length
leader1 = 1
leaderN = 1


pivotLineSegment :: LineSegment 2 () Float -> LineSegment 2 () Float
pivotLineSegment s = OpenLineSegment ((pivotPoint (s^.start.core) pivot a) :+ ()) ((pivotPoint (s^.end.core) pivot a) :+ ())
    where
        pivot = interpolate 0.5 s
        a = atan (((s^.end.core.yCoord) - (s^.start.core.yCoord)) / ((s^.end.core.xCoord) - (s^.start.core.xCoord)))

-- rotates the labels so it is now the top edge
makeTopEdge :: [FSUnplacedLabel] -> LineSegment 2 () Float -> [FSUnplacedLabel]
makeTopEdge ls s = map (pivotLabel pivot a) ls
    where
        pivot = interpolate 0.5 s
        a = atan (((s^.end.core.yCoord) - (s^.start.core.yCoord)) / ((s^.end.core.xCoord) - (s^.start.core.xCoord)))

pivotLabel :: Point 2 Float -> Float -> FSUnplacedLabel -> FSUnplacedLabel
pivotLabel pivot a (p,c) = ((Port (pivotPoint (p^.location) pivot a) (p^.direction) (p^.side)),c)

pivotPoint :: Point 2 Float -> Point 2 Float -> Float -> Point 2 Float
pivotPoint p pivot a = Point2 ((px*c) - (py*s)) ((px*s)+(py*c)) 
    where
        px = p^.xCoord - pivot^.xCoord
        py = p^.yCoord - pivot^.yCoord
        c = cos a
        s = sin a

-- splits a set of labels by edge
splitEdges :: [FSUnplacedLabel] -> Frame -> [[FSUnplacedLabel]]
splitEdges ls f = map (getEdgeLabels ls) (listEdges f)

-- gets the labels for a specific edge from a set of labels
getEdgeLabels :: [FSUnplacedLabel] ->LineSegment 2 () Float -> [FSUnplacedLabel]
getEdgeLabels ls s = filter (labelOnSegment s) ls 

-- determines if a label is on a segment
labelOnSegment :: LineSegment 2 () Float -> FSUnplacedLabel -> Bool
labelOnSegment s l = onSegment ((fst l)^.location) s

-- places labels dynamically for one edge of the boundary


-- placeLabelsDynamic_ :: Int        --index of the first port
--     -> Float
--     -> Int              --index of the second port
--     -> Float
--     -> [UnplacedLabel]
--     -> [Label]
-- placeLabelsDynamic l1 e1 l2 e2 ls
--     | l1 > l2       = []
--     | l1 == l2      = []
--     | l1 + 1 == l2 && 
--         (intersectionLength p1 p2 >= (e2 + (fromIntegral (length (snd (ls!!l2))))*boxSize + 1) 
--         || intersectionLength p2 p1 >= (e1 + (fromIntegral (length (snd (ls!!l1))))*boxSize + 1))
--             = [Label (snd (ls!!l1)) p1 e1,Label (snd (ls!!l2)) p2 e2]
--     | otherwise     =  nub (left splitAt ++ right splitAt)
--         where
--             p1 = (head.fst) (ls!!l1)
--             p2 = (head.fst) (ls!!l2)
--             maxLength l 
--                 | e1 <= e2 = min (min (intersectionLength p1 ((head.fst) (ls!!l))) e1) e2
--                 | otherwise = min (min (intersectionLength p2 ((head.fst) (ls!!l))) e1) e2
--             splitAt
--                 | l1+2==l2  = (l1+1,1)
--                 | otherwise = maximumBy (\y->comparing snd y) ((0,0): filter (\(x,xl)-> (length (left (x,xl)) > 0) && (length (right (x,xl)) > 0)) [(l,el) | l<-[(l1+1)..(l2-1)],el <- [maxLength l]]) 
--             left (0,0) = []
--             left (x,xl) =  (placeLabelsDynamic l1 e1 x xl ls)
--             right (0,0) = []
--             right (x,xl) = (placeLabelsDynamic x xl l2 e2 ls)

--determines the length of l2 is where it crosses l1
intersectionLength :: Port -> Port -> Float
intersectionLength p1 p2 = case ip of
    Just p -> euclideanDist (_location p1) p
    Nothing -> read "Infinity" :: Float
    where ip = asA (intersect (_line p1) (_line p2))