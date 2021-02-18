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

type instance IntersectionOf (ClueBox) ClueBox = [ NoIntersection, Point 2 Float, LineSegment 2 () Float]

instance ClueBox `IsIntersectableWith` ClueBox where
    nonEmptyIntersection = defaultNonEmptyIntersection

    cb `intersect` cb_ =  
     case first List.nub . partitionEithers . mapMaybe collect $ sides of
       ([],[])   -> coRec NoIntersection
       (_, [s])  -> coRec $ first (const ()) s
       ([a],_)   -> coRec a
       ([a,b],_) -> coRec $ ClosedLineSegment (ext a) (ext b)
       (_,_)     -> error "intersecting a line with a triangle. Triangle is degenerate"
     where
       sides = listEdges cb

       collect   :: LineSegment 2 () Float -> Maybe (Either (Point 2 Float) (LineSegment 2 () Float))
       collect s = match (cb_ `intersect` s) $
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
pDummy1 = Port (Point2 128 (-10)) (Vector2 1 (-1)) True
p1 = Port (Point2 128 0) (Vector2 1 1) False -- Green label
p2 = Port (Point2 128 2) (Vector2 1 1) False -- Red label
p3 = Port (Point2 128 4) (Vector2 1 1) False -- Yellow label
p4 = Port (Point2 128 22) (Vector2 1 1) True -- Blue label c and d
p5 = Port (Point2 128 24) (Vector2 1 1) True -- Pink label
p6 = Port (Point2 128 52) (Vector2 1 (-1)) False -- Blue label a and e 
pDummy2 = Port (Point2 128 128) (Vector2 1 1) False

edgeTest = OpenLineSegment ((Point2 128 (-10)) :+ ()) ((Point2 128 128) :+ ()) :: LineSegment 2 () Float

caseA = [([p1],[1]),([p6],[6,6,6])] :: [UnplacedLabel]
caseB = [([p1],[1]),([p2],[2]),([p3],[3,3])] :: [UnplacedLabel]
caseC = [([p1],[1]),([p2],[2]),([p3],[3,3]),([p4],[4,4,4])] :: [UnplacedLabel]
caseE = [([p1],[1]),([p2],[2]),([p3],[3,3]),([p6],[6,6])] :: [UnplacedLabel]


-- dynamic labeling pipeline
dynamicPipeLine :: [UnplacedLabel] -> Frame -> IO()
dynamicPipeLine labels frame = do
    -- pick a port for each label
    let aLabels = assignPorts labels
    putStrLn $ "assigned ports: " ++ show (length aLabels)

    -- simplify the frame to only boundary
    let simpleFrame = simplify $ listEdges frame
    putStrLn $ "simplified frame to: " ++ show (length simpleFrame)

    -- place labels for each edge
    mapM (placeLabelsDynamicEdgeIO aLabels) simpleFrame
    -- putStrLn $ "placed labels: " ++ show labels

    return ()

placeLabelsDynamicEdgeIO :: [FSUnplacedLabel] -> LineSegment 2 () Float -> IO()
placeLabelsDynamicEdgeIO labels edge = do
    putStrLn $ "edge:" ++ show edge
    -- rotate line segment edge so it is horizontal
    let s = pivotLineSegment edge
    putStrLn $ "rotated edge:" ++ show s

    -- get labels for the edge
    let edgeLabels = getEdgeLabels labels edge
    putStrLn $ "labels for this edge:" ++ show edgeLabels

    -- pivot labels so they match the pivoted edge
    let pivotedLabels = makeTopEdge edgeLabels edge
    putStrLn $ "pivoted labels:" ++ show pivotedLabels

    -- add dummy labels
    let allLabels = dummy0 s : pivotedLabels ++ [dummyNplus1 s]
    putStrLn $ "with dummy labels: " ++ show allLabels

    -- place the labels
    placeLabelsDynamicEdgeIO_ allLabels 0 ((length allLabels)-1) 1000 1000


    return ()
placeLabelsDynamicEdgeIO_ :: [FSUnplacedLabel] -> Int -> Int -> Int -> Int -> IO()
placeLabelsDynamicEdgeIO_ ls p1 p2 e1 e2 = do
    putStrLn $ "amount labels;" ++ show (length ls)
    putStrLn $ "p1: " ++ show p1
    putStrLn $ "p2: " ++ show p2
    putStrLn $ "e1: " ++ show e1
    putStrLn $ "e2: " ++ show e2
    let f i j a b 
                | i == j - 1 || ((length m) == 0) = (a + b,(0,0))
                | j - 1 > i = minimum m
                    where 
                        m = [(fst (f i k a c) + fst (f k j c b) - c,(k,c)) | k<-[i+1..j-1], c<- [0..max 1 (min a b)], elem c (set k), valid i a j b k c]
                        set k = take (((p2-p1)^2) + 2) [e*(boxSize+2)|e <- [0..]]
                        valid i a j b k c = fitLength (fst (ls!!i)) a (fst (ls!!j)) b (fst (ls!!k)) c

    let r = array((p1,p1),(p2,p2)) [((i,j),(f i j e1 e2))|i<-[p1..(p2-1)],j<-[(p1+1)..p2]]
    putStrLn $ "result: " ++ show ((e1:getLengths r p1 p2)++[e2])
 

-- places labels dynamically
placeLabelsDynamic :: [UnplacedLabel] -> Frame -> [Label] 
placeLabelsDynamic ls f = concat $ map (placeLabelsDynamicEdge ls_) (simplify $ listEdges f)
    where ls_ = assignPorts ls

simplify :: [LineSegment 2 () Float] -> [LineSegment 2 () Float]
simplify ls
    | abs (getM l - getM (last ll)) < 0.01 || getM l == getM (last ll) = (((last ll)&start .~ (last ll)^.start)&end .~ l^.end) : (init ll)
    | otherwise = (l:ll)
    where (l:ll) = simplify_ ls


simplify_ :: [LineSegment 2 () Float] -> [LineSegment 2 () Float]
simplify_ [l] = [l]
simplify_ (l:ll:ls)
    | abs (getM l - getM ll) < 0.01 || getM l == getM ll = simplify_ (((l&start .~ l^.start)&end .~ ll^.end) : ls)
    | otherwise = l:(simplify_ (ll:ls))

getM :: LineSegment 2 () Float -> Float
getM l = (l^.end.core.yCoord - l^.start.core.yCoord) / (l^.end.core.xCoord - l^.start.core.xCoord)

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
    
minExtLength :: [Port] -> Int -> Int -> Int -> Int -> Int
minExtLength ls p1 p2 e1 e2 = r!(p1,p2)
    where
            r = array((p1,p1),(p2,p2)) [((i,j),(f i j e1 e2))|i<-[p1..p2],j<-[p1..p2]]                                                      -- minimum total extension length for ports [p1..p2]
            f i j a b                                                                                                                       -- T[i,j,a,b]
                | i == j - 1 = a + b
                | j - 1 > 1 = minimum [(f i k a c) + (f k j c b) - c | k<-[i+1..j-1], c <- [0.. (min a b)], elem c (set k), valid i a j b k c]
                | otherwise = error "j = i"
            set k = [0..(p2-p1)^2]
            valid i a j b k c = fitLength (ls!!i) a (ls!!j) b (ls!!k) c

-- placeLabelsDynamicEdge_ :: [Port] -> Int -> Int -> Int -> Int -> Array (Int, Int) (Int, (Int, Int))
placeLabelsDynamicEdge_ ls p1 p2 e1 e2 = e1:getLengths r p1 p2++[e2]
    where
            r = array((p1,p1),(p2,p2)) [((i,j),(f i j e1 e2))|i<-[p1..p2],j<-[p1..p2]]
            f i j a b 
                | i == j - 1 || ((length m) == 0) = (a + b,(0,0))
                | j - 1 > 1 = minimum m
                    where m = [(fst (f i k a c) + fst (f k j c b) - c,(k,c)) | k<-[i+1..j-1], c<- [0..max 1 (min a b)], elem c (set k), valid i a j b k c]
            set k = take (((p2-p1)^2) + 2) [e*(boxSize+2)|e <- [0..]]
            valid i a j b k c = fitLength (fst (ls!!i)) a (fst (ls!!j)) b (fst (ls!!k)) c

-- test_ = placeLabelsDynamicEdge_ [pDummy1,p1,p2,p3,p4,pDummy2] 2 3 1 1

-- getLengths :: (Ix t, Eq a1, Num t, Num a1) => Array (t, t) (a2, (t, a1)) -> t -> t -> [a1]
getLengths r p1 p2
    | p1 == p2 || snd l == (0,0) = []
    | otherwise = getLengths r p1 (fst (snd l)) ++ [ snd (snd l)] ++ getLengths r (fst (snd l)) p2
    where l = r!(p1,p2)
 

-- determines if a leader with length len fits between two ports with lengths len1 and len2
-- fitLength :: Port -> Integer -> Port -> Integer -> Port -> Integer -> Bool
fitLength :: Port -> Int -> Port -> Int -> Port -> Int -> Bool
fitLength (Port pos1 dir1 s1) len1 (Port pos2 dir2 s2) len2 (Port pos dir s) len 
    | pos1 == pos || pos2 == pos = True
    | intersects l1 l = False
    | intersects l2 l = False
    | intersects b l1 = False
    | intersects b l2 = False
    | intersects b1 l = False
    | intersects b2 l = False
    | intersects b1 b = False
    | intersects b2 b = False
    | otherwise = True
    where
        l1 = leader pos1 dir1 len1
        l2 = leader pos2 dir2 len2
        l = leader pos dir len
        b1 = clueBoxPolygon (l1^.end.core) dir1 s1
        b2 = clueBoxPolygon (l2^.end.core) dir2 s2
        b = clueBoxPolygon (l^.end.core) dir s

intersectsTrace a b  = trace ((show a)++(show b)) (intersects a b)

toLine :: LineSegment 2 () Float -> Line 2 Float
toLine ls = lineThrough (ls^.start.core) (ls^.end.core)

leader :: Point 2 Float -> Vector 2 Float -> Int -> LineSegment 2 () Float
leader p v l =  ClosedLineSegment (p :+ ()) (q :+ ())
    where
    q = p .+^ ((signorm v)^*(fromIntegral l))

clueBoxPolygon :: Point 2 Float -> Vector 2 Float -> Bool -> ClueBox
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
lineIntersection :: Float -> Float -> Float -> Float -> (Float,Float)
lineIntersection m1 b1 m2 b2 = (x,m1*x + b1)
    where
        x = (b2-b1)/(m1-m2)

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

-- pivot the line segment so it is horizontal
pivotLineSegment :: LineSegment 2 () Float -> LineSegment 2 () Float
pivotLineSegment s =  inty (OpenLineSegment ((pivotPoint (s^.start.core) pivot a) :+ ()) ((pivotPoint (s^.end.core) pivot a) :+ ()))
    where
        pivot = interpolate 0.5 s
        a = acos (dot (lineSegmentDirection s) (Vector2 (-1) 0))

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
makeTopEdge :: [FSUnplacedLabel] -> LineSegment 2 () Float -> [FSUnplacedLabel]
makeTopEdge ls s = map (pivotLabel pivot a) ls
    where
        pivot = interpolate 0.5 s
        a = atan (((s^.end.core.yCoord) - (s^.start.core.yCoord)) / ((s^.end.core.xCoord) - (s^.start.core.xCoord)))

pivotLabel :: Point 2 Float -> Float -> FSUnplacedLabel -> FSUnplacedLabel
pivotLabel pivot a (p,c) = ((Port (pivotPoint (p^.location) pivot a) (pivotVector (p^.direction) pivot a) (p^.side)),c)

pivotPoint :: Point 2 Float -> Point 2 Float -> Float -> Point 2 Float
pivotPoint p pivot a = Point2 ((px*c) - (py*s) + pivot^.xCoord) ((px*s)+(py*c) + pivot^.yCoord) 
    where
        px = p^.xCoord - pivot^.xCoord
        py = p^.yCoord - pivot^.yCoord
        c = cos a
        s = sin a

pivotVector :: Vector 2 Float -> Point 2 Float -> Float -> Vector 2 Float
pivotVector p pivot a = Vector2 ((px*c) - (py*s)) ((px*s)+(py*c)) 
    where
        px = p^.xComponent - pivot^.xCoord
        py = p^.yComponent - pivot^.yCoord
        c = cos a
        s = sin a

-- splits a set of labels by edge
splitEdges :: [FSUnplacedLabel] -> Frame -> [[FSUnplacedLabel]]
splitEdges ls f = map (getEdgeLabels ls) (listEdges f)

-- gets the labels for a specific edge from a set of labels
getEdgeLabels :: [FSUnplacedLabel] ->LineSegment 2 () Float -> [FSUnplacedLabel]
getEdgeLabels ls s = sortBy sortLabel (filter (labelOnSegment s) ls)

sortLabel :: FSUnplacedLabel -> FSUnplacedLabel -> Ordering
sortLabel (p1,_) (p2,_)
    | p1^.location < p2^.location = LT
    | otherwise = GT

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