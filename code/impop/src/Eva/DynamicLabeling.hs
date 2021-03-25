{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Eva.DynamicLabeling where

import Data.List hiding (intersect)
import Data.List.Split
import Debug.Trace
import Data.Ord
import Data.Vinyl hiding (Label)
import Data.Vinyl.CoRec
import Data.Ext

import Data.Array((!),array)

import Control.Lens
import Control.Monad.Zip

import Data.Geometry hiding (head,direction,init,replicate,unit)
import Data.Geometry.PlanarSubdivision hiding(location)
--import Data.Geometry.Ipe
import Data.Geometry.Transformation
import Data.Geometry.Matrix

import CurveArrangement
import CurveArrangement.Types

import Nonogram

import           Data.Geometry.Polygon
import qualified Data.List as List
import           Data.Bifunctor
import           Data.Either (partitionEithers)
import           Data.Maybe (mapMaybe)

import Debug.Trace

_line :: Port -> Line 2 Float
_line p = Data.Geometry.Line (_location p) (Nonogram._direction p)

--Test
-- pDummy1 = Port (Point2 0 0) (Vector2 1 (-1)) True
-- p1 = Port (Point2 0 2) (Vector2 1 1) True
-- p2 = Port (Point2 0 4) (Vector2 1 1) False
-- p3 = Port (Point2 0 7) (Vector2 1 1) True
-- p4 = Port (Point2 0 9) (Vector2 1 1) False
-- pDummy2 = Port (Point2 0 12) (Vector2 1 1) False
-- p1 = Point2 0 0
-- p2 = Point2 20 0
-- p3 = Point2 20 20
-- p4 = Point2 0 20
-- p5 = Point2 0 (-20)
-- p6 = Point2 20 (-20)
-- p7 = Point2 (-20) (-20)
-- p8 = Point2 40 20
-- ls1 = ClosedLineSegment (p1 :+ ()) (p3 :+ ()) :: LineSegment 2 () Float
-- ls2 = ClosedLineSegment (p1 :+ ()) (p4 :+ ()) :: LineSegment 2 () Float
-- ls3 = ClosedLineSegment (p5 :+ ()) (p4 :+ ()) :: LineSegment 2 () Float
-- ls4 = ClosedLineSegment (p1 :+ ()) (p3 :+ ()) :: LineSegment 2 () Float
-- ls5 = ClosedLineSegment (p6 :+ ()) (p3 :+ ()) :: LineSegment 2 () Float
-- ls6 = ClosedLineSegment (p5 :+ ()) (p8 :+ ()) :: LineSegment 2 () Float
-- ls7 = ClosedLineSegment (p4 :+ ()) (p8 :+ ()) :: LineSegment 2 () Float
-- ls8 = ClosedLineSegment (p7 :+ ()) (p3 :+ ()) :: LineSegment 2 () Float
-- lss = [ls1,ls2,ls3,ls4,ls5,ls6,ls7,ls8]
-- lsss = lss ++ (map flipSegment lss)

-- test1 = [([p1],(1) (2)::[Int]),([p2],(2) (3)::[Int]),([p3],[1]::[Int]),([p4],[1]::[Int]),([p5],[1]::[Int])]

width = 600

height = 600

boxSize = 16

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

--------------------------------------------------------------------------------
-- Clue box type: rectangle that is not orthogonal
type ClueBox = SimplePolygon () Float

type instance IntersectionOf (ClueBox) (LineSegment 2 () Float) = [ NoIntersection, Point 2 Float, LineSegment 2 () Float]

instance (ClueBox) `IsIntersectableWith` (LineSegment 2 () Float) where
    nonEmptyIntersection = defaultNonEmptyIntersection

    cb `intersect` ls =
     case first List.nub . partitionEithers . mapMaybe collect $ sides of
       ([],[])   -> coRec NoIntersection
       ([], [s])  -> coRec NoIntersection -- $ first (const ()) s    Technically intersects but one line segment is ok
       ([a],_)   -> coRec a
       ([a,b],_) -> coRec $ ClosedLineSegment (ext a) (ext b)
       (_,_)     -> error "intersecting a line with a box. Box is degenerate"
     where
       sides = listEdges cb

       collect   :: LineSegment 2 () Float -> Maybe (Either (Point 2 Float) (LineSegment 2 () Float))
       collect s = match (s `intersect` ls) $
                        (H $ \NoIntersection           -> Nothing)
                     :& (H $ \(a :: Point 2 Float)         -> Just $ Left a)
                     :& (H $ \(e :: LineSegment 2 () Float) -> Just $ Right e)
                     :& RNil

type instance IntersectionOf (ClueBox) ClueBox = [ NoIntersection, Point 2 Float, LineSegment 2 () Float]

instance ClueBox `IsIntersectableWith` ClueBox where
    nonEmptyIntersection = defaultNonEmptyIntersection

    cb `intersect` cb_ =  
     case first List.nub . partitionEithers . mapMaybe collect $ sides of
       ([],[])      -> coRec NoIntersection 
       ([], [s])    -> coRec NoIntersection                 -- One line segment intersecting is OK
       ([a,b],_)    -> coRec $ ClosedLineSegment (ext a) (ext b)
       (_, (s:_))   -> coRec $ first (const ()) s
       ([a],[])     -> coRec NoIntersection                   -- One corner intersecting is OK
       (_,_)        -> error "intersecting a line with a triangle. Triangle is degenerate"
     where
       sides = listEdges cb

       collect   :: LineSegment 2 () Float -> Maybe (Either (Point 2 Float) (LineSegment 2 () Float))
       collect s = match (cb_ `intersect_` s) $
                        (H $ \NoIntersection           -> Nothing)
                     :& (H $ \(a :: Point 2 Float)         -> Just $ Left a)
                     :& (H $ \(e :: LineSegment 2 () Float) -> Just $ Right e)
                     :& RNil
       cb `intersect_` ls =
        case first List.nub . partitionEithers . mapMaybe collect $ sides of
            ([],[])   -> coRec NoIntersection
            (_, [s])  -> coRec $ first (const ()) s
            ([a],_)   -> coRec a
            ([a,b],_) -> coRec $ ClosedLineSegment (ext a) (ext b)
            (_,_)     -> error "intersecting a line with a box. Box is degenerate"
            where
                sides = listEdges cb

                collect   :: LineSegment 2 () Float -> Maybe (Either (Point 2 Float) (LineSegment 2 () Float))
                collect s = match (s `intersect` ls) $
                                    (H $ \NoIntersection           -> Nothing)
                                :& (H $ \(a :: Point 2 Float)         -> Just $ Left a)
                                :& (H $ \(e :: LineSegment 2 () Float) -> Just $ Right e)
                                :& RNil


--------------------------------------------------------------------------

unit = 3

frame_ :: SimplePolygon () Float
frame_ = fromPoints [Point2 (112.0) (-128.0) :+ (),Point2 (128.0) (-128.0) :+ (),Point2 (128.0) (-96.0) :+ (),Point2 (128.0) (128.0) :+ (),Point2 (16.0) (128.0) :+ (),Point2 (-16.0) (128.0) :+ (),Point2 (-128.0) (128.0) :+ (),Point2 (-128.0) (-48.0) :+ (),Point2 (-128.0) (-96.0) :+ (),Point2 (-128.0) (-128.0) :+ ()]

uls :: [UnplacedLabel]
uls = [([Port {_location = Point2 (1.36705) (-128.0), Nonogram._direction = Vector2 (-0.37423944) (-0.92733216), _side = True},Port {_location = Point2 (104.69) (128.0), Nonogram._direction = Vector2 (0.37427685) (0.9273171), _side = False}],[3]),
    ([Port {_location = Point2 (47.8395) (-128.0), Nonogram._direction = Vector2 (0.35125157) (-0.9362811), _side = True},Port {_location = Point2 (-48.2005) (128.0), Nonogram._direction = Vector2 (-0.35125062) (0.9362815), _side = False}],[1]),
    ([Port {_location = Point2 (71.5081) (-128.0), Nonogram._direction = Vector2 (-0.2693775) (-0.9630347), _side = True},Port {_location = Point2 (128.0) (73.9608), Nonogram._direction = Vector2 (0.26937416) (0.9630357), _side = False}],[1]),
    ([Port {_location = Point2 (128.0) (-76.244), Nonogram._direction = Vector2 (0.90954244) (-0.41561103), _side = True},Port {_location = Point2 (-128.0) (40.7337), Nonogram._direction = Vector2 (-0.9095437) (0.41560838), _side = False}],[1]),
    ([Port {_location = Point2 (128.0) (-56.2167), Nonogram._direction = Vector2 (0.6422371) (-0.7665061), _side = True},Port {_location = Point2 (-26.3526) (128.0), Nonogram._direction = Vector2 (-0.64224195) (0.766502), _side = False}],[3]),
    ([Port {_location = Point2 (128.0) (73.9608), Nonogram._direction = Vector2 (0.26937416) (0.9630357), _side = True},Port {_location = Point2 (71.5081) (-128.0), Nonogram._direction = Vector2 (-0.2693775) (-0.9630347), _side = False}],[0]),
    ([Port {_location = Point2 (128.0) (96.264), Nonogram._direction = Vector2 (0.9586338) (0.2846425), _side = True},Port {_location = Point2 (-128.0) (20.2512), Nonogram._direction = Vector2 (-0.95863426) (-0.28464067), _side = False}],[3]),
    ([Port {_location = Point2 (128.0) (113.105), Nonogram._direction = Vector2 (0.9783424) (0.20699327), _side = True},Port {_location = Point2 (-128.0) (58.9404), Nonogram._direction = Vector2 (-0.9783414) (-0.20699772), _side = False}],[1]),
    ([Port {_location = Point2 (104.69) (128.0), Nonogram._direction = Vector2 (0.37427685) (0.9273171), _side = True},Port {_location = Point2 (1.36705) (-128.0), Nonogram._direction = Vector2 (-0.37423944) (-0.92733216), _side = False}],[1]),
    ([Port {_location = Point2 (-26.3526) (128.0), Nonogram._direction = Vector2 (-0.64224195) (0.766502), _side = True},Port {_location = Point2 (128.0) (-56.2167), Nonogram._direction = Vector2 (0.6422371) (-0.7665061), _side = False}],[1]),
    ([Port {_location = Point2 (-48.2005) (128.0), Nonogram._direction = Vector2 (-0.35125062) (0.9362815), _side = True},Port {_location = Point2 (47.8395) (-128.0), Nonogram._direction = Vector2 (0.35125157) (-0.9362811), _side = False}],[3]),
    ([Port {_location = Point2 (-128.0) (58.9404), Nonogram._direction = Vector2 (-0.9783414) (-0.20699772), _side = True},Port {_location = Point2 (128.0) (113.105), Nonogram._direction = Vector2 (0.9783424) (0.20699327), _side = False}],[0]),
    ([Port {_location = Point2 (-128.0) (40.7337), Nonogram._direction = Vector2 (-0.9095437) (0.41560838), _side = True},Port {_location = Point2 (128.0) (-76.244), Nonogram._direction = Vector2 (0.90954244) (-0.41561103), _side = False}],[3]),
    ([Port {_location = Point2 (-128.0) (20.2512), Nonogram._direction = Vector2 (-0.95863426) (-0.28464067), _side = True},Port {_location = Point2 (128.0) (96.264), Nonogram._direction = Vector2 (0.9586338) (0.2846425), _side = False}],[1])]

testPs = [Port {_location = Point2 (1.36705) (-128.0), Nonogram._direction = Vector2 (-0.37423944) (-0.92733216), _side = False},Port {_location = Point2 (71.5081) (-128.0), Nonogram._direction = Vector2 (-0.2693775) (-0.9630347), _side = False},Port {_location = Point2 (47.8395) (-128.0), Nonogram._direction = Vector2 (0.35125157) (-0.9362811), _side = True},Port {_location = Point2 (47.8395) (-128.0), Nonogram._direction = Vector2 (0.35125157) (-0.9362811), _side = False}]

pos :: Point 2 Float
[Port pos1 dir1 s1,Port pos dir s,Port pos2 dir2 s2,Port pos3 dir3 s3] = testPs
m = 0.766502 / (-0.64224195) ::Float
m2 = (-0.9362811)/0.35125157 ::Float
m1 = (-0.92733216) / (-0.37423944) ::Float
b = pos^.yCoord
b2 = pos2^.yCoord


-- cases from nono labeling pdf figure 6
pDummy1 = Port (Point2 0 0) (Vector2 (-1) (1)) False
p1 = Port (Point2 0 0) (Vector2 1 1) True -- Green label
p2 = Port (Point2 1 0) (Vector2 1 1) True -- Red label
p3 = Port (Point2 2 0) (Vector2 1 1) True -- Yellow label
p4 = Port (Point2 28 0) (Vector2 1 1) False -- Blue label c and d
p5 = Port (Point2 32 0) (Vector2 1 1) False -- Pink label
p6 = Port (Point2 110 0) (Vector2 (-1) 1) True -- Blue label a and e 
pDummy2 = Port (Point2 128 0) (Vector2 1 1) True

edgeTest = OpenLineSegment ((Point2 128 0) :+ ()) ((Point2 0 0) :+ ()) :: LineSegment 2 () Float
frameTest = fromPoints [(Point2 128 0) :+ (),(Point2 0 0) :+ (),(Point2 0 (-128)) :+ (),(Point2 128 (-128)) :+ ()] :: SimplePolygon () Float

caseA = [([p6],[6,6,6]),([p1],[1,1,1,1])] :: [UnplacedLabel]
caseB = [([p3],[3,3]),([p2],[2]),([p1],[1])] :: [UnplacedLabel]
caseC = [([p5],[5]),([p4],[4,4,4]),([p3],[3,3]),([p2],[2]),([p1],[1])] :: [UnplacedLabel]
caseE = [([p6],[6,6]),([p3],[3,3]),([p2],[2]),([p1],[1])] :: [UnplacedLabel]


-- places labels dynamically
placeLabelsDynamic :: [UnplacedLabel] -> Frame -> [Label] 
placeLabelsDynamic ls f = concatMap (placeLabelsDynamicEdge ls_) (simplify $ listEdges f)
    where ls_ = assignPorts ls

-- simplify a list of line segments by merging parallel lines that share an endpoint
simplify 
    :: [LineSegment 2 () Float] -- List of line segments
    -> [LineSegment 2 () Float] -- Simplified list of line segments
simplify ls
    | abs (getM l - getM (last ll)) < 0.01 || getM l == getM (last ll) = (((last ll)&start .~ (last ll)^.start)&end .~ l^.end) : init ll
    | otherwise = l:ll
    where (l:ll) = simplify_ ls

simplify_ :: [LineSegment 2 () Float] -> [LineSegment 2 () Float]
simplify_ [l] = [l]
simplify_ (l:ll:ls)
    | abs (getM l - getM ll) < 0.01 || getM l == getM ll = simplify_ (((l&start .~ l^.start)&end .~ ll^.end) : ls)
    | otherwise = l:simplify_ (ll:ls)

-- get the slope of a line segment
getM :: LineSegment 2 () Float -> Float
getM l = (l^.end.core.yCoord - l^.start.core.yCoord) / (l^.end.core.xCoord - l^.start.core.xCoord)

-- Places labels dynamically on an edge, it is assumed the edge is horizontal and the labels extend upwards
placeLabelsDynamicEdge 
    :: [FSUnplacedLabel]        -- List of unplaced labels with fixed side assignment
    -> LineSegment 2 () Float   -- The edge on which labels are placed
    -> [Label]                  -- The placed labels
placeLabelsDynamicEdge labels edge = zipWith placeLabel edgeLabels lengths
    where
        lengths = getLengths placedLabels 0 (length allLabels - 1)
        placedLabels = placeLabelsDynamicEdge_ allLabels 0 (length allLabels - 1) 1000 1000
        allLabels = dummy0 s : makeTopEdge edgeLabels m mv ++ [dummyNplus1 s] -- Rotated labels with added dummies
        s = transformBy m edge
        edgeLabels = getEdgeLabels labels edge
        m = toBaseTransformation edge
        mv = transformOriginV (toVectorBase edge)

-- place the label
placeLabel 
    :: FSUnplacedLabel  -- The unplaced label
    -> Int              -- The extension length
    -> Label            -- The placed label
placeLabel ul e = Label (snd ul) (fst ul) (fromIntegral e)

-- placeLabelsDynamicEdge_ :: [Port] -> Int -> Int -> Int -> Int -> Array (Int, Int) (Int, (Int, Int))
placeLabelsDynamicEdge_ ls p1 p2 e1 e2 = 
   let f i j a b 
                | i == j - 1  = (a + b,(0,0))
                | null m = (1000000,(-1 * boxSize,0)) 
                | j - 1 > i = minimum m
                    where 
                        m = [(fst (f i k a c) + fst (f k j c b) - c,(k,c)) | k<-[i+1..j-1], c<-set k , valid i a j b k c ]
                        set k = filter (\x-> x < max 1 (min a b)) (take (max (p2-p1 + 2) 5) [e*boxSize|e <- [0..]] )
                        valid i a j b k c = fitLength (fst (ls!!i)) (a + boxLength (ls!!i))  (fst (ls!!j)) (b + boxLength (ls!!j)) (fst (ls!!k)) (c + boxLength (ls!!k))
                        boxLength l = boxSize * length (snd l)
    in array((p1,p1),(p2,p2)) [((i,j),f i j e1 e2)|i<-[p1..p2],j<-[p1..p2]]

-- getLengths :: (Ix t, Eq a1, Num t, Num a1) => Array (t, t) (a2, (t, a1)) -> t -> t -> [a1]
getLengths r p1 p2
    | port == -1 = replicate (p2-p1 + 1) (-1)
    | (port,l)== (0,0) = []
    | otherwise = getLengths r p1 port ++ [l] ++ getLengths r port p2
    where (_,(port,l)) = r!(p1,p2)

debug = flip trace
 

-- Determines if l' fits in between l1 and l2
fitLength 
    :: Port     -- Port of l1
    -> Int      -- Extension length of l1
    -> Port     -- Port of l2
    -> Int      -- Extension length of l2
    -> Port     -- Port of l'
    -> Int      -- Extension length of l'
    -> Bool
fitLength (Port pos1 dir1 s1) len1 (Port pos2 dir2 s2) len2 (Port pos dir s) len 
    | pos1 == pos || pos2 == pos = True -- `debug` "true: positions are the same"
    | intersects l1 l = False -- `debug` "false: i intersects k"
    | intersects l2 l = False -- `debug` "false: j intersects k"
    | intersects b l1 = False -- `debug` "false: box k intersects i"
    | intersects b l2 = False -- `debug` "false: box k intersects j"
    | intersects b1 l = False -- `debug` "false: box i intersects k"
    | intersects b2 l = False -- `debug` "false: box j intersects k"
    | intersects b1 b = False -- `debug` ("false: box i " ++ show b1 ++ " intersects box k" ++ show b)
    | intersects b2 b = False -- `debug` "false: box j intersects box k"
    | otherwise = True -- `debug` "true: no intersection"
    where
        l1 = (leader pos1 dir1 len1) --`debug` ("i: " ++ show (leader pos1 dir1 len1))
        l2 = (leader pos2 dir2 len2) --`debug` ("j: " ++ show (leader pos2 dir2 len2)) 
        l = (leader pos dir len) --`debug` ("k: " ++ show (leader pos dir len))
        b1 = clueBoxPolygon (l1^.end.core) dir1 s1
        b2 = clueBoxPolygon (l2^.end.core) dir2 s2
        b = clueBoxPolygon (l^.end.core) dir s

intersectsTrace a b  = trace ((show a)++(show b)) (intersects a b)

-- convert a line segment to a line
toLine :: LineSegment 2 () Float -> Line 2 Float
toLine ls = lineThrough (ls^.start.core) (ls^.end.core)

-- create a line segment that represents a leader, from a given starting position, direction and length
leader :: Point 2 Float -> Vector 2 Float -> Int -> LineSegment 2 () Float
leader p v l =  ClosedLineSegment (p :+ ()) (q :+ ())
    where
    q = p .+^ ((signorm v)^*(fromIntegral l))

-- Create a clue box for line l
clueBoxPolygon 
    :: Point 2 Float    -- Attachment point
    -> Vector 2 Float   -- Direction of l
    -> Bool             -- Side that must be labeled. True if on right of direction vector (left if going into puzzle)
    -> ClueBox
clueBoxPolygon p v False = fromPoints [p :+ (),p2 :+ (),p3 :+ (),p4 :+ ()]
    where
        ub = signorm v^*(fromIntegral boxSize)
        iv = inverseVector ub
        p2 = p .+^ ub
        p3 = p2 .+^ (Vector2 (-iv^.xComponent) (iv^.yComponent))
        p4 = p3 .+^ (negated ub)
clueBoxPolygon p v True = fromPoints [p :+ (),p2 :+ (),p3 :+ (),p4 :+ ()]
    where
        ub = signorm v^*(fromIntegral boxSize)
        iv = inverseVector ub
        p2 = p .+^ ub
        p3 = p2 .+^ (Vector2 (iv^.xComponent) (-iv^.yComponent))
        p4 = p3 .+^ (negated ub)

inverseVector :: Vector 2 Float -> Vector 2 Float
inverseVector v = Vector2 (v^.yComponent) (v^.xComponent)

-- intersection point
-- y = m1*x + b1
-- y = m2*x + b2
-- m1*x + b1 = m2*x + b2
-- m1*x - m2*x = b2 - b1
-- (m1-m2)*x = b2 - b1
-- x = (b2-b1)/(m1-m2) 
-- y = m1*x + b1

-- Intersection point line l1:m1*x + b1 and l2:m2*x + b2
lineIntersection :: Float -> Float -> Float -> Float -> (Float,Float)
lineIntersection m1 b1 m2 b2 = (x,m1*x + b1)
    where
        x = (b2-b1)/(m1-m2)

-- Assigns ports to unplaced labels 
assignPorts :: [UnplacedLabel] -> [FSUnplacedLabel]
assignPorts [] = []
assignPorts ((ps,c):ls) =  (head ps, c):assignPorts ls
assignPorts_ [] = []
assignPorts_ ((ps,c):ls) 
    | length ps > 1 = (ps!!1, c):assignPorts ls
    | otherwise = (head ps, c):assignPorts ls

-- initialize leader lengths
initLeaders l = (maxLength:leader1:take (l-2) (repeat 0))++ [leaderN,maxLength]
    where maxLength = l^2 -- maxLength of leader

-- Makes dummies, assumes line segment is horizontal
dummy0 :: LineSegment 2 () Float -> FSUnplacedLabel
dummy0 s = (Port (Point2 (((s^.start.core.xCoord)) - unit) ((s^.start.core.yCoord))) (Vector2 (unit) unit) True,[0])
dummyNplus1 :: LineSegment 2 () Float -> FSUnplacedLabel
dummyNplus1 s = (Port (Point2 ((s^.end.core.xCoord) - unit) (s^.end.core.yCoord)) (Vector2 (-unit) unit) False,[0])

-- Sets start and end leader length
leader1 = 1
leaderN = 1


--interpolate the y coordinates of a line segment
inty ls = OpenLineSegment ((Point2 x1 y) :+ ()) ((Point2 x2 y) :+ ())
    where 
        x1 = ls^.start.core.xCoord
        x2 = ls^.end.core.xCoord
        y1 = ls^.start.core.yCoord
        y2 = ls^.end.core.yCoord
        y = (y1+y2) / 2

lineSegmentDirection :: LineSegment 2 () Float -> Vector 2 Float
lineSegmentDirection ls = signorm (Vector2 ((ls^.end.core.xCoord) - (ls^.start.core.xCoord)) ((ls^.end.core.yCoord) - (ls^.start.core.yCoord)))

-- rotates the labels so it is now the top edge
makeTopEdge 
    :: [FSUnplacedLabel]        -- The unplaced labels
    -> Transformation 2 Float   -- The translation transformation
    -> Transformation 2 Float   -- The rotation transformation
    -> [FSUnplacedLabel]
makeTopEdge ls m mv = map (transformLabel m mv) ls

transformLabel :: Transformation 2 Float -> Transformation 2 Float -> FSUnplacedLabel -> FSUnplacedLabel
transformLabel m mv (p,c) = ((Port (transformBy m (p^.location))  (transformBy mv (p^.direction)) (p^.side)),c)

orig = Point2 0 0

-- splits a set of labels by edge
splitEdges :: [FSUnplacedLabel] -> Frame -> [[FSUnplacedLabel]]
splitEdges ls f = map (getEdgeLabels ls) (listEdges f)

-- gets the labels for a specific edge from a set of labels
getEdgeLabels :: [FSUnplacedLabel] ->LineSegment 2 () Float -> [FSUnplacedLabel]
getEdgeLabels ls s = filter (labelOnSegment s) ls

sortLabel :: FSUnplacedLabel -> FSUnplacedLabel -> Ordering
sortLabel (p1,_) (p2,_)
    | p1^.location < p2^.location = LT
    | otherwise = GT

-- determines if a label is on a segment
labelOnSegment :: LineSegment 2 () Float -> FSUnplacedLabel -> Bool
labelOnSegment s l = onSegment ((fst l)^.location) s

--determines the length of l2 is where it crosses l1
intersectionLength :: Port -> Port -> Float
intersectionLength p1 p2 = case ip of
    Just p -> euclideanDist (_location p1) p
    Nothing -> read "Infinity" :: Float
    where ip = asA (intersect (_line p1) (_line p2))


rotationMatrix :: Float -> Transformation 2 Float
rotationMatrix a = Transformation . Matrix $ Vector3 (Vector3 (cos a)       (sin a) 0)
                                                     (Vector3 (-(sin a))    (cos a) 0)
                                                     (Vector3 0             0       1)

-- Transformation matrix for a new basis defined by a point and a vector (direction of new x-axis)
transformOrigin :: Point 2 Float -> Vector 2 Float -> Transformation 2 Float
transformOrigin p v = transformOriginV v |.| transformOriginP p

transformOriginP :: Point 2 Float -> Transformation 2 Float
transformOriginP p = Transformation . Matrix $ Vector3  (Vector3 1 0 (-p^.xCoord))
                                                        (Vector3 0 1 (-p^.yCoord))
                                                        (Vector3 0 0 1)

transformOriginV :: Vector 2 Float -> Transformation 2 Float
transformOriginV v = Transformation . Matrix $ Vector3 (Vector3 (-v^.xComponent) (-v^.yComponent) 0)
                                                        (Vector3 (v^.yComponent) (-v^.xComponent)  0)
                                                        (Vector3 0          0           1)

toVectorBase :: LineSegment 2 () Float -> Vector 2 Float
toVectorBase ls = signorm (ls^.end.core .-. ls^.start.core)

-- Transformation for translation to the origin
toBaseTransformation :: LineSegment 2 () Float -> Transformation 2 Float
toBaseTransformation ls = transformOrigin (ls^.end.core) (toVectorBase ls)