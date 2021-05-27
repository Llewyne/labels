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

import Nonogram

import Data.Geometry.Polygon

import Eva.Util
import Eva.ClueBox hiding (boxSize,debug_log,debug)
import Eva.Leader hiding (boxSize,debug_log,debug)
import Eva.Test
import SAT.Mios (CNFDescription (..), solveSAT)

type FSUnplacedLabel = (Port, Clue) --Unplaced Label with a fixed side

boxSize = 16
unit = 3

debug_log = True

small_number = 0.0001

-- places labels dynamically
placeLabelsDynamic :: [UnplacedLabel] -> Frame -> IO [Label] 
placeLabelsDynamic ls f = do
    mapM_ (checkValid f) ls
    ls_ <- assignPorts ls
    let edges = listEdges f --`debug` show ls_
    let roundedEdges = map roundSegment edges --`debug` show (length edges)
    let filteredEdges = removeZeroLength roundedEdges
    let simplified = simplify filteredEdges --`debug` show (simplify filteredEdges)
    return $ concatMap (placeLabelsDynamicEdge ls_) (simplify simplified)
    
-- Check if an unplaced label is valid
checkValid :: Frame -> UnplacedLabel -> IO ()
checkValid f ul 
    | all (isOnFrame f) (fst ul) = return ()
    | otherwise = error ("not all ports are on the frame: " ++ show ul ++ show f)

-- Determines if a port lies on the frame
isOnFrame :: Frame -> Port -> Bool
isOnFrame f p = (p^.location) `intersects` f

assignPorts :: [UnplacedLabel] -> IO [FSUnplacedLabel]
assignPorts ul = do
    -- Create a variable for each port
    let ml = mapVars ul
    -- mapM_ (putStrLn . show) ml

    -- Set the clauses
    let clauses = setClauses ml
    --putStrLn $ show clauses

    -- Create a problem description for the SAT solver
    let desc = CNFDescription (length $ concat ml) (length clauses) ""
    -- Solve
    asg <- solveSAT desc clauses

    --putStrLn $ show asg

    return (map (assignPort (concat ml)) $ filter (0<) asg)
              
assignPort :: [(Int,(UnplacedLabel,Int))] -> Int -> FSUnplacedLabel
assignPort ml i = (port, clue)
        where
            clue = snd $ fst $ snd $ ml!!(i-1)
            port = (fst $ fst $ snd $ ml!!(i-1))!!((snd $ snd $ ml!!(i-1))-1)


-- Sets the clauses
-- for each line l with ports p and q, xal = 1 (xal = 0) if the label above l is assigned to p (q). 
-- And xbl = 1 (xbl = 0) if the label below l is assigned to p (q)
-- If a label cant fit next to another label because the box would intersect the leader
-- then  the clause: -xal or -xb'l is added
-- also for each line l the clauses: xal or xbl, -xal or -xbl to ensure each line has exactly one label above and below
setClauses :: [[(Int,(UnplacedLabel,Int))]] -> [[Int]]
setClauses ml = map (\((a,_),(b,_))->[-a,-b]) (filter (not.fitBox) (filter differentPort (pairs (concat ml)))) -- clauses for overlapping 
            ++ concat ( map (\ls->[[-(fst $ ls!!0),-(fst $ ls!!1)],[(fst $ ls!!0),(fst $ ls!!1)]]) ( filter (\x->length x == 2) ml)) -- make sure 1 of 2 ports is selected for every line
            ++ map (\ls->[fst $ head ls]) (filter (\x->length x == 1) ml) 

-- Determine if ports are unequal
differentPort :: ((Int,(UnplacedLabel,Int)),(Int,(UnplacedLabel,Int)))-> Bool
differentPort ((_,l1),(_,l2)) = (_location $ getPort l1) /= (_location $ getPort l2)

--Shorthand for getting the ith possible port from an unplaced label
getPort :: (UnplacedLabel,Int) -> Port
getPort ((ps,_),i) = ps!!(i-1)

--All possible pairs from a list
pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

-- map the variables for the SAT clauses,
-- result is a list of pairs where the first element is the index of the variable and the second element the port assignment
mapVars :: [UnplacedLabel] -> [[(Int,(UnplacedLabel,Int))]]
mapVars ul = _mapVars (map getVar ul) 1

_mapVars :: [[(UnplacedLabel,Int)]] -> Int -> [[(Int,(UnplacedLabel,Int))]]
_mapVars [] _ = []
_mapVars (v:vs) i = zip [i..] v : _mapVars vs (i + (length v))

-- Create variables for every port of an unplaced label
getVar :: UnplacedLabel -> [(UnplacedLabel,Int)]
getVar ul = [(a,b)|a<-[ul],b<-[1..(length (fst ul))] ]

-- Places labels dynamically on an edge, it is assumed the edge is horizontal and the labels extend upwards
placeLabelsDynamicEdge 
    :: [FSUnplacedLabel]        -- List of unplaced labels with fixed side assignment
    -> LineSegment 2 () Float   -- The edge on which labels are placed
    -> [Label]                  -- The placed labels
placeLabelsDynamicEdge labels edge = zipWith placeLabel edgeLabels lengths `debug` (show (edge,edgeLabels,allLabels)) --`debug` (show edge ++ "\n" ++ show s ++ "\n" ++ show mv ++ "\n" ++ show edgeLabels ++ "\n" ++ show allLabels)
    where
        edgeLabels = getEdgeLabels labels edge                                             -- The labels on this edge
        lengths = map snd $ sort $ zip sorting (map (snd) (tail $ init (snd placedLabels))) `debug` (show (placedLabels))
        placedLabels = placeLabelsDynamicEdge_ allLabels 0 ((length allLabels) - 1) 100 100 `debug` ("length alllabels - 1: " ++ show ((length allLabels) -1))
        (sorting,allLabels) = prepareEdgeLabels edgeLabels edge

-- place the label
placeLabel 
    :: FSUnplacedLabel  -- The unplaced label
    -> Int              -- The extension length
    -> Label            -- The placed label
placeLabel ul e = Label (snd ul) (fst ul) (fromIntegral e)

-- prepare the labels for the dynamic programming algorithm:
-- - Transform the edge so it is horizontal, starts at (0,0) and a top edge (for ease of calculation)
-- - Transform the labels onto the transformed edge
-- - Sort the labels on x coordinate
-- - Add dummy labels
-- - Remember the original sorting so the resulting extension lengths can be put in the right order
prepareEdgeLabels :: [FSUnplacedLabel] -> LineSegment 2 () Float -> ([Int],[FSUnplacedLabel])
prepareEdgeLabels labels edge = (ordering, preparedLabels)
    where
        m                   = toBaseTransformation edge                                             -- The transformation matrix for transforming the edge and port positions
        tEdge               = transformBy m edge    	                                            -- The transformed edge
        mv                  = transformOriginV (toVectorBase edge)                                  -- The transformation matrix for transforming the port directions
        tLabels             = makeTopEdge labels m mv                                               -- The transformed labels
        (ordering,sLabels)  = unzip $ sortBy compareLabels $ zip [0..] tLabels                      -- The sorted labels and ordering
        preparedLabels      = dummy0 tEdge : sLabels ++ [dummyNplus1 tEdge]                         -- The prepared labels


compareLabels (_,((Port l1 _ s1),_)) (_,((Port l2 _ s2),_)) = (l1^.xCoord,i s1) `compare` (l2^.xCoord,i s2)
    where
        i s | s = 1
            | otherwise = 0
               


-- placeLabelsDE 
--     :: [FSUnplacedLabel] -- The unplaced labels
--     -> [(Int, Int)]        -- List of label indexes and corresponding length
-- placeLabelsDE ls = lengths
--     where
--         lengths = getLengths_ r!!(0,length ls - 1,100,100)
--         r = array()


-- getLengths _ = [(0,0)]

-- placeLabelsDynamicEdge_ :: [Port] -> Int -> Int -> Int -> Int -> Array (Int, Int) (Int, (Int, Int))
placeLabelsDynamicEdge_ ls p1 p2 e1 e2 = (f p1 p2 e1 e2)
   where 
       f i j a b 
                | i == j - 1  = (a+b,[(i,a),(j,b)]) -- `debug` ("I AND J ADJACENT|| i: " ++ show i ++ ", j: " ++ show j ++ ", a: " ++ show a ++ ", b: " ++ show b)
                | null m = (1000000,[(i,a)] ++ [(k,-boxSize)|k<-[i+1..j-1]] ++[(j,b)]) `debug` ("M NULL|| i: " ++ show (i,ls!!i) ++ ", j: " ++ show (j,ls!!j) ++ ", a: " ++ show a ++ ", b: " ++ show b)
                | j - 1 > i = minimum m --`debug` ("OTHER|| m: " ++ show m ++ "min m: " ++ show (minimum m) ++ "i: " ++ show (i,ls!!i) ++ ", j: " ++ show (j,ls!!j) ++ ", a: " ++ show a ++ ", b: " ++ show b)
                    where 
                        m = [(fst (f i k a c) + fst (f k j c b) - c,init (snd (f i k a c)) ++ snd (f k j c b)) | k<-[i+1..j-1], c<-set k , valid i a j b k c ] --`debug` (show ls ++ show i ++ show j ++ show (set 1))
                        set k = [minLength (fst (ls!!k)) + e | e <- [0..(max a b)+boxSize], e == 0 || e `mod` boxSize == 0] 
                        valid i a j b k c = fitLength ls i a j b k c
-- Determines the minimum length for the label to clear the boundary
minLength :: Port -> Int
minLength (Port p d s) | ((x > 0 && s) || (x < 0 && not s))= minlength --`debug` (show minlength ++ ", " ++ show p ++ show d ++ show s)  -- direction is tot the top right and label is on right side
                       | otherwise = 0 --`debug` ("minlength 0" ++ show p ++ show d ++ show s)
                            where
                                x = d^.xComponent
                                y = d^.yComponent 
                                minlength = ceiling (fromIntegral boxSize / ((abs y) / (abs x)))
                       

-- getLengths :: (Ix t, Eq a1, Num t, Num a1) => Array (t, t) (a2, (t, a1)) -> t -> t -> [a1]
getLengths r p1 p2
    | port == -1 = replicate (p2-p1 + 1) (-1)
    | (port,l)== (0,0) = []
    | otherwise = getLengths r p1 port ++ [l] ++ getLengths r port p2
    where (_,(port,l)) = r!(p1,p2) `debug` show r

debug x y | debug_log = flip trace x y
        | otherwise = x
 
-- Determines if l' might fit with to l1
fitBox :: ((Int,(UnplacedLabel,Int)),(Int,(UnplacedLabel,Int)))-> Bool
fitBox ((_,(p1,i1)),(_,(p2,i2))) 
    | pos1 == pos || pos2 == pos = True -- `debug` "true: positions are the same"
    | intersects l1 l = False -- `debug` "false: i intersects k"
    | intersects b l1 = False -- `debug` "false: box k intersects i"
    | otherwise = True -- `debug` "true: no intersection"
    where
        l1 = (leader pos1 dir1 boxSize) --debug` ("i: " ++ show (leader pos1 dir1 boxSize))
        l = (leader pos dir boxSize) --`debug` ("k: " ++ show (leader pos dir boxSize))
        b = clueBoxPolygon (l^.end.core) dir s 1
        Port pos1 dir1 s1 = getPort (p1,i1)
        Port pos dir s = getPort (p2,i2)

-- Determines if l' fits in between l1 and l2
fitLength 
    :: [FSUnplacedLabel] -- labels
    -> Int     -- index of l1
    -> Int      -- Extension length of l1
    -> Int     -- index of l2
    -> Int      -- Extension length of l2
    -> Int     -- index of l'
    -> Int      -- Extension length of l'
    -> Bool
fitLength ls i len1 j len2 k len = not (lb1 `intersects` lb) && not (lb2 `intersects` lb)
    where
        lb1 = leaderFromLabelLength (ls!!i) len1 --`debug` ("i:" ++ show (leaderFromLabelLength (ls!!i) len1) ++ show len1)
        lb2 = leaderFromLabelLength (ls!!j) len2 --`debug` ("j:" ++ show (leaderFromLabelLength (ls!!j) len2) ++ show len2)
        lb = leaderFromLabelLength (ls!!k) len --`debug` ("k:" ++ show (leaderFromLabelLength (ls!!k) len) ++ show len)

--     | intersects l1 l = False  -- `debug` "false: i intersects k"
--     | intersects l2 l = False  -- `debug` "false: j intersects k"
--     | intersects b l1 = False  -- `debug` "false: box k intersects i"
--     | intersects b l2 = False  -- `debug` "false: box k intersects j"
--     | intersects b1 l = False  -- `debug` "false: box i intersects k"
--     | intersects b2 l = False  -- `debug` "false: box j intersects k"
--     | intersects b1 b = False  -- `debug` ("false: box i " ++ show b1 ++ " intersects box k" ++ show b)
--     | intersects b2 b = False  -- `debug` "false: box j intersects box k"
--     | otherwise = True  -- `debug` "true: no intersection"
--     where
--         l1 = (leader pos1 dir1 len1) `debug` ("i: " ++ show (i,(leader pos1 dir1 len1)) ++ ", j: " ++ show (j,(leader pos2 dir2 len2))  ++ ", k: " ++ show (k,(leader pos dir len)))
--         l2 = (leader pos2 dir2 len2)
--         l = (leader pos dir len) 
--         b1 = clueBoxPolygon (l1^.end.core) dir1 s1
--         b2 = clueBoxPolygon (l2^.end.core) dir2 s2
--         b = clueBoxPolygon (l^.end.core) dir s
--         (Port pos1 dir1 s1) = fst $ ls!!i
--         (Port pos2 dir2 s2) = fst $ ls!!j
--         (Port pos dir s) = fst $ ls!!k

intersectsTrace a b  = trace ((show a)++(show b)) (intersects a b)

-- initialize leader lengths
initLeaders l = (maxLength:leader1:take (l-2) (repeat 0))++ [leaderN,maxLength]
    where maxLength = l^2 -- maxLength of leader

-- Makes dummies, assumes line segment is horizontal
dummy0 :: LineSegment 2 () Float -> FSUnplacedLabel
dummy0 s = (Port (Point2 ((s^.start.core.xCoord) - unit) (s^.start.core.yCoord)) (Vector2 (-unit) 0) False,[0])
dummyNplus1 :: LineSegment 2 () Float -> FSUnplacedLabel
dummyNplus1 s = (Port (Point2 ((s^.end.core.xCoord) + unit) (s^.end.core.yCoord)) (Vector2 (unit) 0) True,[0])

-- Sets start and end leader length
leader1 = 1
leaderN = 1

-- rotates the labels so it is now the top edge
makeTopEdge 
    :: [FSUnplacedLabel]        -- The unplaced labels
    -> Transformation 2 Float   -- The translation transformation
    -> Transformation 2 Float   -- The rotation transformation
    -> [FSUnplacedLabel]
makeTopEdge ls m mv = map (transformLabel m mv) ls

transformLabel :: Transformation 2 Float -> Transformation 2 Float -> FSUnplacedLabel -> FSUnplacedLabel
transformLabel m mv (p,c) = (Port (transformBy m (p^.location))  (transformBy mv (p^.direction)) (p^.side),c)

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

leaderFromLabelLength :: FSUnplacedLabel -> Int -> Leader
leaderFromLabelLength (Port p v s, c) i = (ls, clueBoxPolygon (ls^.end.core) v s (length c))
    where ls = leader p v i

--Cluebox on the boundary
clueBoxFromLabel :: FSUnplacedLabel -> ClueBox
clueBoxFromLabel (Port p v s,c) = clueBoxPolygon p v s (length c)

------------------

rotationFrame = fromPoints [(Point2 128 (-128)) :+ (),(Point2 (-128) (-128)) :+ (),(Point2 (-128) (128)) :+ (),(Point2 128 (128)) :+ ()] :: SimplePolygon () Float

testRotation = mapM (testRotationEdge testLabels) (listEdges rotationFrame)
testPoint1 = Point2 10 128
testPoint2 = Point2 128 (-10)
testPoint3 = Point2 (-10) (-128)
testPoint4 = Point2 (-128) 10

testDirection1 = Vector2 1 1
testDirection2 = Vector2 1 (-1)
testDirection3 = Vector2 (-1) 1
testDirection4 = Vector2 (-1) (-1)

testPorts = [Port testPoint1 testDirection3 False, Port testPoint1 testDirection3 True,Port testPoint1 testDirection1 False, Port testPoint1 testDirection1 True,
                Port testPoint2 testDirection1 False, Port testPoint2 testDirection1 True,Port testPoint2 testDirection2 False, Port testPoint2 testDirection2 True,
                Port testPoint3 testDirection2 False, Port testPoint3 testDirection2 True,Port testPoint3 testDirection4 False, Port testPoint3 testDirection1 True,
    	        Port testPoint4 testDirection4 False, Port testPoint4 testDirection4 True,Port testPoint4 testDirection3 False, Port testPoint4 testDirection3 True]
testLabels = map (\p->(p,[0])) testPorts

testRotationEdge ls e = do
    putStrLn $ show e
    let m = toBaseTransformation e
    let mv = transformOriginV (toVectorBase e)
    let s = transformBy m e 
    putStrLn $ show s
    putStrLn "----------------------------------"
    let eL = getEdgeLabels ls e
    putStrLn $ show eL
    let rL = makeTopEdge eL m mv
    putStrLn $ show rL
    putStrLn "----------------------------------"
    putStrLn "----------------------------------"

