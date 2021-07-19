{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Eva.DynamicLabeling where

import Data.List hiding (intersect)
import qualified Data.List (intersect)
import Data.List.Split
import Debug.Trace
import Data.Ord
import Data.Vinyl hiding (Label)
import Data.Vinyl.CoRec
import Data.Ext
import Data.Maybe

import Data.Array((!),array)

import Control.Lens
import Control.Monad.Zip

import Data.Geometry hiding (head,direction,init,replicate,unit)
import Data.Geometry.PlanarSubdivision hiding(location)

import Nonogram

import Data.Geometry.Polygon

import Eva.Util
import Eva.NonoUtil
import Eva.ClueBox hiding (debug_log,debug)
import Eva.Leader hiding (boxSize,debug_log,debug)
import Eva.Test
import Eva.Intersect hiding (FSUnplacedLabel)
import Eva.Assignment

import SAT.Mios (CNFDescription (..), solveSAT)

import System.Exit

type FSUnplacedLabel = (Port, Clue) --Unplaced Label with a fixed side


unit = 3

debug_log = False

small_number = 0.0001

-- places labels dynamically
placeLabelsDynamic :: [UnplacedLabel] -> Frame -> IO [Label] 
placeLabelsDynamic ls f = do
    mapM_ (checkValid f) ls
    let edges = listEdges f --`debug` show ls_
    let roundedEdges = map roundSegment edges --`debug` show (length edges)
    let filteredEdges = removeZeroLength roundedEdges
    let simplified = simplify filteredEdges --`debug` show (simplify filteredEdges)
    ls_ <- assignPorts ls (simplify simplified)
    findSolution ls_ [] 0 (simplify simplified) -- $ concatMap (placeLabelsDynamicEdge ls_) ( simplify simplified)
    
-- Tries all assignments until a solution is found
findSolution :: [[FSUnplacedLabel]] -> [Label] -> Int-> [LineSegment 2 () Float]  -> IO [Label]
findSolution uls best total ls = do
    putStrLn $ "Trying assignment number: " ++ (show $ length uls)
    let amountLabels = length $ head uls
    let sol = concatMap(placeLabelsDynamicEdge (head uls)) ls
    let labeled = length (filter (\x -> x^.offset >= 0) sol)
    let bestNew = if labeled > total then sol else best
    let bestLabeled = if labeled > total then labeled else total
    if labeled  == amountLabels || length uls == 1 then return bestNew else findSolution (tail uls) bestNew bestLabeled ls

-- Check if an unplaced label is valid
checkValid :: Frame -> UnplacedLabel -> IO ()
checkValid f ul 
    | all (isOnFrame f) (fst ul) = return ()
    | otherwise = error ("not all ports are on the frame: " ++ show ul ++ show f)

-- Determines if a port lies on the frame
isOnFrame :: Frame -> Port -> Bool
isOnFrame f p = (p^.location) `intersects` f

assignPorts :: [UnplacedLabel] -> [LineSegment 2 () Float] -> IO [[FSUnplacedLabel]]
assignPorts ul ls = do
    -- Create a variable for each port
    let ml = mapVars ul
    mapM_ (putStrLn . show) ml

    -- Set the clauses
    let clauses = setClauses ml ls
    putStrLn $ show clauses

    -- Create a problem description for the SAT solver
    let desc = CNFDescription (length $ concat ml) (length clauses) ""
    -- Solve
    asg <- solveSAT desc clauses

    putStrLn $ show asg

    -- die (show asg)

    other <- getAllOtherSolutions clauses [asg] (length $ concat ml) 1000
    shuffled <- shuffle other
    let random100 = take 100 shuffled
    putStrLn $ "Total possible assignments found:" ++ (show $ length random100)

    let assignedPorts = map (filter (0<)) random100
    return $ map (map (assignPort (concat ml))) assignedPorts

-- Gets all other solutions for a SAT problem
getAllOtherSolutions :: [[Int]] -> [[Int]] -> Int -> Int -> IO [[Int]]
getAllOtherSolutions cls sols vars limit = do
    if length sols == limit 
        then return sols 
        else do
            let sol = map ((-1)*) (head sols) -- clause to ban previously found solution
            let newcls = sol:cls -- add to existing clauses
            let desc = CNFDescription vars (length newcls) ""
            asg <- solveSAT desc newcls -- solve new problem

            if null asg then return sols else getAllOtherSolutions newcls (asg:sols) vars limit -- stop when no solution can be found
              
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
-- If it is impossible for a label to intersect another, make sure it is always chosen (when it is the first or last port on a frame edge and is pointed away from the other labels)
-- also for each line l the clauses: xal or xbl, -xal or -xbl to ensure each line has exactly one label above and below
setClauses :: [[VarAssignment]] -> [LineSegment 2 () Float] -> [[Int]]
setClauses ml l  = --map (\((a,_),(b,_))->[-a,-b]) (filter (not.(fitBox l)) (filter differentPort (pairs (concat ml)))) -- `debug` show (map (\((a,_),(b,_))->[-a,-b]) (filter differentPort (pairs (concat ml))))-- clauses for overlapping 
            concat ( map (\ls->[[-(fst $ ls!!0),-(fst $ ls!!1)],[(fst $ ls!!0),(fst $ ls!!1)]]) ( filter (\x->length x == 2) ml)) -- make sure 1 of 2 ports is selected for every line
            ++ map (\ls->[fst $ head ls]) (filter (\x->length x == 1) ml) -- If there is only one port, it must be chosen
            ++ map alwaysFitClause duplicatesRemovedSet
            
            where

                duplicatesRemovedSet = nubBy (\x y -> x == y || x == reverse y) alwaysFitSet -- [[VarAssignment]]
                alwaysFitSet = concat $ map isOutAndPointedAway_ groupedByEdge -- [[VarAssignment]]
                groupedByEdge = map (groupByEdge ml_) l -- [[[VarAssignment]]]
                ml_ = ml ++ map reverse ml -- [[VarAssignment]]


-- Return only the assignments on ls 
groupByEdge :: [[VarAssignment]] -> LineSegment 2 () Float -> [[VarAssignment]]
groupByEdge mls ls = sortBy (compareVA ls) $ filter (isOnEdge_ ls) mls 
    where isOnEdge_ ls (va:_) = isOnEdge ls ((getPortVA va)^.location)
        



-- Order of points on the line segment
compareVA :: LineSegment 2 () Float -> [VarAssignment] -> [VarAssignment] -> Ordering
compareVA ls va1 va2 = _compareVA ls (head va1) ( head va2)
    where 
        _compareVA :: LineSegment 2 () Float -> VarAssignment -> VarAssignment -> Ordering
        _compareVA ls v1 v2 = (euclideanDist (ls^.end.core) l1, i s1) `compare` (euclideanDist (ls^.end.core) l2, i s2) --`debug` (show $ ls^.start.core)
            where
                (Port l1 _ s1) = getPortVA v1
                (Port l2 _ s2) = getPortVA v2
                i s | s = 1
                    | otherwise = 0

getLabelFromVa :: VarAssignment -> UnplacedLabel
getLabelFromVa va = fst $ snd va

alwaysFitClause 
    :: [VarAssignment] -- Set of vars for one port where the first port always fits
    -> [Int] -- Clause
alwaysFitClause (ml:mls) = map (negate . fst) mls -- The other option for this label should never be chosen
 

isOutAndPointedAway_
    :: [[VarAssignment]] -- Vars for this edge
    -> [[VarAssignment]]
isOutAndPointedAway_ vars = filter ((isOutAndPointedAway) vars) vars --`debug` (show $ map (getPortVA . head) (filter ((isOutAndPointedAway) vars) vars))

isOutAndPointedAway 
    :: [[VarAssignment]]  -- mapped labels for this frame edge
    -> [VarAssignment] -- a label
    -> Bool -- True if first or last label on edge and pointed away
isOutAndPointedAway mle ml = np > pi / 2.0 --`debug` ((show $ getPortVA $ head ml) ++ show np)
    where
        Port p v _ = getPortVA ml_
        np
            | head mle == ml && length mle > 3 = angleBetweenVectors v (((getPortVA $ head (mle!!2))^.location) .-. p) --`debug` (show (((getPortVA $ head (mle!!1))^.location) .-. p))
            | last mle == ml && length mle > 3 = angleBetweenVectors (((getPortVA $ head (mle!!(length mle - 3)))^.location) .-. p) v --`debug` (show (((getPortVA $ head (mle!!(length mle - 2)))^.location) .-. p))
            | otherwise = 0
        ml_ = ml!!0

-- Determine if ports are unequal
differentPort :: ((Int,(UnplacedLabel,Int)),(Int,(UnplacedLabel,Int)))-> Bool
differentPort ((_,l1),(_,l2)) = (_location $ getPort l1) /= (_location $ getPort l2)

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
        lengths = map snd $ sort $ zip sorting (map (snd) (tail $ init (snd placedLabels))) `debug` (show (placedLabels) ++ show sorting ++ show allLabels)
        placedLabels = placeLabelsDynamicEdge_ allLabels 0 ((length allLabels) - 1) 0 0 `debug` ("length alllabels - 1: " ++ show ((length allLabels) -1))
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
                | m == [] || ((snd $ head m) == []) = (1000000,[(i,a)] ++ [(k,-boxSize)|k<-[i+1..j-1]] ++[(j,b)]) --`debug` ("M NULL|| i: " ++ show (i,ls!!i) ++ ", j: " ++ show (j,ls!!j) ++ ", a: " ++ show a ++ ", b: " ++ show b)
                | j - 1 > i = minimum m --`debug` ("OTHER|| m: " ++ show m ++ "min m: " ++ show (minimum m) ++ "i: " ++ show (i,ls!!i) ++ ", j: " ++ show (j,ls!!j) ++ ", a: " ++ show a ++ ", b: " ++ show b)
                    where 
                        m = [smart i k j a b c | k<-[i+1..j-1],c <- [set k i j a b]] --`debug` (show ls ++ show i ++ show j ++ show (set 1)) 
                        set k i j a b = chs blocking --`debug` ("result:" ++ show (chs blocking) ++ "k:" ++ show (k,ls!!k) ++ " i:" ++ show (i,a,ls!!i) ++ " j:" ++ show (j,b,ls!!j))
                            where 
                                chs [] = 1000000
                                chs a = maximum a
                                blocking = catMaybes [minBlockingLength2 ls i a k, minBlockingLength2 ls j b k, minBoundaryBlockingLength ls i j k a b]

                        smart i k j a b c 
                            | c == 1000000      =   (1000000,[])
                            | fst f1 >= 1000000 =   (1000000,[])
                            | fst f2 >= 1000000 =   (1000000,[])
                            | otherwise = ((fst f1) + (fst f2) - c, init (snd f1) ++ snd f2) `debug` show (c,f1,f2)
                                where
                                    f1 = f i k a c
                                    f2 =  f k j c b

-- getLengths :: (Ix t, Eq a1, Num t, Num a1) => Array (t, t) (a2, (t, a1)) -> t -> t -> [a1]
getLengths r p1 p2
    | port == -1 = replicate (p2-p1 + 1) (-1)
    | (port,l)== (0,0) = []
    | otherwise = getLengths r p1 port ++ [l] ++ getLengths r port p2
    where (_,(port,l)) = r!(p1,p2) --`debug` show r

debug x y | debug_log = flip trace x y
        | otherwise = x
 



intersectsTrace a b  = trace ((show a)++(show b)) (intersects a b)

-- initialize leader lengths
initLeaders l = (maxLength:leader1:take (l-2) (repeat 0))++ [leaderN,maxLength]
    where maxLength = l^2 -- maxLength of leader

-- Makes dummies, assumes line segment is horizontal
dummy0 :: LineSegment 2 () Float -> FSUnplacedLabel
dummy0 s = (Port (Point2 ((s^.end.core.xCoord) - unit) (s^.end.core.yCoord)) (Vector2 (-unit) (-unit)) False,[])
dummyNplus1 :: LineSegment 2 () Float -> FSUnplacedLabel
dummyNplus1 s = (Port (Point2 ((s^.start.core.xCoord) + unit) (s^.start.core.yCoord)) (Vector2 (unit) (-unit)) True,[])

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
labelOnSegment s l = isOnEdge s ((fst l)^.location)



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

