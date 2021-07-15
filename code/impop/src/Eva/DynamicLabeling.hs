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
import Eva.ClueBox hiding (boxSize,debug_log,debug)
import Eva.Leader hiding (boxSize,debug_log,debug)
import Eva.Test
import SAT.Mios (CNFDescription (..), solveSAT)

import System.Exit

type FSUnplacedLabel = (Port, Clue) --Unplaced Label with a fixed side
type PortAssignment = (UnplacedLabel,Int) -- Possible assignment where the integer is the index of the assigned port of the unplaced label
type VarAssignment = (Int,PortAssignment) -- Possible port assignment mapped to an index  

getPortVA (_,pa) = getPort pa

boxSize = 16
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
        
isOnEdge ls p
            | p == (ls^.end.core) = False -- make sure each port is only on one edge
            | otherwise = p `onSegment` ls


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

minBoundaryBlockingLength :: [FSUnplacedLabel] -> Int -> Int -> Int -> Int -> Int ->  Maybe Int
minBoundaryBlockingLength ls i j k a b
    | i == 0 && j == (length ls) - 1 = Just m -- Can't be blocked by dummies
    | i == 0 && fitLength_ ls k m j b = Just m
    | fitLength_ ls i a k m && j == (length ls) - 1 = Just m
    | fitLength_ ls i a k m && fitLength_ ls k m j b =  Just m 
    | otherwise = Nothing
    where
        m = minLength (fst (ls!!k)) --`debug` show (i,a,j,b,k,minLength (fst (ls!!k)))


-- Determines the minimum length for the label to clear the boundary
-- Assumes boundary is horizontal
minLength :: Port -> Int
minLength (Port p d s) | ((x > 0 && s) || (x < 0 && not s))= minlength --`debug` (show minlength ++ ", " ++ show p ++ show d ++ show s)  -- direction is tot the top right and label is on right side
                       | otherwise = 0 --`debug` ("minlength 0" ++ show p ++ show d ++ show s)
                            where
                                x = d^.xComponent
                                y = d^.yComponent 
                                minlength = ceiling (fromIntegral boxSize / ((abs y) / (abs x)))

minLengthEdge :: Port -> LineSegment 2 () Float -> Int
minLengthEdge prt@(Port p d s) ls
    | (angle_ < (pi/2) && s) || (angle_ > (pi/2) && not s) = 0
    | ls^.start.core.yCoord == ls^.end.core.yCoord && d^.yComponent > 0 = minLength prt -- horizontal
    | otherwise = ceiling ((fromIntegral boxSize) / tan angle)
        where
            angle 
                | angle_ <= (pi/2) = angle_
                | otherwise = abs $ angleBetweenVectors (ls^.start.core .-. ls^.end.core) d
            angle_ = abs $ angleBetweenVectors (ls^.end.core .-. ls^.start.core) d

minBlockingLength2 
  :: [FSUnplacedLabel]    -- the labels
    -> Int                  -- index of label i
    -> Int                  -- The length of label i
    -> Int                  -- index of label k
    -> Maybe Int
minBlockingLength2 ls i a k 
    | i == 0 || i == (length ls) = Nothing -- Dummies can't block
    |  (k < i && not sk) || (k > i && sk) = Nothing
    | otherwise = lengthFromIntersection2 vk pk vvk ip
    where
        ip = findIntersection line_k line_k_ ( (catMaybes [ip_ik_,ip_itop_k_,ip_opp_k_])++[pi_opp,pi_top])

        li@(Port pi vi si,c) = ls!!i
        lk@(Port pk vk sk,_) = ls!!k

        line_k = lineFromVectorPoint vk pk
        line_k_ =  lineFromVectorPoint vk pk_                                   -- line defined by side of cluebox opposite of extension=
        line_i =  lineFromVectorPoint vi pi     
        line_i_ = Line pi_bot vi
        line_i_top =  lineThrough pi_top pi_opp                                    -- line defined by extension

        --The blocking intersection point is either on one of the top corners of the i clue box
        --or is the (highest) intersection of line k' with one of the line segments making up the  i clue box

        ip_ik_ = asA @(Point 2 Float) $ line_i_ `intersect` line_k_
        ip_itop_k_ = asA @(Point 2 Float) $ ls_i_top `intersect` line_k_
        ip_opp_k_ = asA @(Point 2 Float) $ ls_i_opp `intersect` line_k_

        ls_i = ClosedLineSegment (ext pi) (ext pi_top)                             -- Line segment from port to end of label (including extension)
        ls_i_top = ClosedLineSegment (ext pi_top) (ext pi_opp)                       -- Line segment on top of box
        ls_i_opp = ClosedLineSegment (ext pi_opp) (ext pi_bot)                       -- Line segment on opposite of box

        pk_ = pk .+^ ((signorm vvk)^*(fromIntegral boxSize))
        pi_top = pi .+^ ((signorm vi)^* (fromIntegral(a + ((length c)* boxSize))))   -- Point on end of extension (including box)
        pi_opp = pi_top .+^ ((signorm vvi)^*(fromIntegral boxSize))                  -- Point on top of label opposite extension
        pi_bot= pi_opp .+^ ((signorm $ negated vi)^*(fromIntegral ((length c)* boxSize))) 

        vvi 
            | si = Vector2 (vi^.yComponent) (-(vi^.xComponent))
            | otherwise = Vector2 (-(vi^.yComponent)) (vi^.xComponent)
        vvk 
            | sk = Vector2 (vk^.yComponent) (-(vk^.xComponent))
            | otherwise = Vector2 (-(vk^.yComponent)) (vk^.xComponent)

lengthFromIntersection2 :: Vector 2 Float -> Point 2 Float -> Vector 2 Float -> Maybe (Point 2 Float) -> Maybe Int
lengthFromIntersection2 _ _ _ Nothing = Nothing
lengthFromIntersection2 vk pk vvk (Just ip) = lengthFromIntersection2_ corner
    where 
        corner = asA @(Point 2 Float) $ (lineFromVectorPoint vvk ip) `intersect` Line pk vk
        lengthFromIntersection2_ (Just c) = Just (ceiling $ euclideanDist pk c)
        lengthFromIntersection2_ Nothing = Nothing

--highest intersection between k and k'
findIntersection :: Line 2 Float -> Line 2 Float -> [Point 2 Float] -> Maybe (Point 2 Float)
findIntersection line_k line_k_ is
    | length inBetweens > 0 = Just (getHighestPoint_ inBetweens (head inBetweens))
    | otherwise = Nothing
    where --there is a bug in onSide and onLine, so onLine2 is neccesary
        inBetweens = filter (\x-> x^.yCoord >= 0 && (x `onLine2` line_k || x `onLine2` line_k_ || x `onSide` line_k /= x `onSide` line_k_)) is



-- i blocks k:
-- only if label of k is flipped towards i
-- otherwise length of k at intersection (maybe + boxSize)
minBlockingLength 
    :: [FSUnplacedLabel]    -- the labels
    -> Int                  -- index of label i
    -> Int                  -- The length of label i
    -> Int                  -- index of label k
    -> Maybe Int
minBlockingLength ls i a k 
    | (k < i && not sk) || (k > i && sk)                = Nothing `debug` "Label faces outward, cant be blocked"
    | si == sk && dontIntersect ip1                     = Nothing `debug` "labels on same side and intersection below B"
    | si /= sk && dontIntersect ip5                     = Nothing `debug` "labels facing inwards and intersection below B"
    | ls_i_top `intersects` line_k_                    = lengthFromIntersection pk_ ip2 `debug` "k' intersects top i"
    | si == sk && ((i < k && (angleBetweenVectors vi vk) > 0) || (k > i && (angleBetweenVectors vk vi) > 0))       = lengthFromIntersection pi_ ip3 `debug` "labels on same side and angle > 0"
    | si == sk && ((i < k && (angleBetweenVectors vi vk) < 0) || (k > i && (angleBetweenVectors vk vi) < 0))       = lengthFromIntersection pi__ ip4 `debug` "labels on the same side and angle < 0"
    | si == sk && line_i `intersects` line_k_            = lengthFromIntersection pk_ ip1 `debug` "labels on same side and k' intersects lsi"
    | si /= sk && ls_i `intersects` line_k_ && (angleBetweenVectors vi vk) >= 90 = lengthFromIntersection pk_ ip1 `debug` "labels facing inwards and lsi intersects k' and angle >= 90"
    | si /= sk && ls_i `intersects` line_k_            = lengthFromIntersection pi_ ip3 `debug` "labels facing inwards and lsi intersects k' and angle <90"
    | si /= sk && line_i_ `intersects` line_k_         = lengthFromIntersection pk_ ip5 `debug` "labels facing inwards and i' intersects k'"
    | otherwise = error ("missed some case" ++ show i ++ "|" ++ show a ++ "|" ++ show k ++ "|" ++ show line_i_ ++ show line_k_ ++ show ip1 ++ show ls_i ++ (show (ls_i `intersect` line_k_)))
    where
        li@(Port pi vi si,c) = ls!!i
        lk@(Port pk vk sk,_) = ls!!k
        line_k_ =  lineFromVectorPoint vk pk_                                   -- line defined by side of cluebox opposite of extension=
        line_i =  lineFromVectorPoint vi pi     
        line_i_ = lineFromVectorPoint vi pi___           
        line_i_top =  lineThrough pi_ pi__                                    -- line defined by extension
                     -- line defined by extension
        line_pi_ = lineFromVectorPoint vk pi_ 
        line_pi__ = lineFromVectorPoint vk pi__
        line_pi___ = lineFromVectorPoint vk pi___


        pi_ = pi .+^ ((signorm vi)^* (fromIntegral(a + ((length c)* boxSize))))   -- Point on end of extension (including box)
        pi__ = pi_ .+^ ((signorm vvi)^*(fromIntegral boxSize))                  -- Point on top of label opposite extension
        pi___= pi__ .+^ ((signorm $ negated vi)^*(fromIntegral ((length c)* boxSize)))        -- Point on bottom of label

        ls_i = OpenLineSegment (ext pi) (ext pi_)                             -- Line segment from port to end of label (including extension)
        ls_i_top = OpenLineSegment (ext pi_) (ext pi__)                       -- Line segment on top of box
        ls_i_opp = OpenLineSegment (ext pi__) (ext pi___)                       -- Line segment on opposite of box

        ip1 = asA @(Point 2 Float) $ line_k_ `intersect` line_i
        ip2 = asA @(Point 2 Float) $ line_k_ `intersect` line_i_top
        ip3 = asA @(Point 2 Float) $ line_pi_ `intersect` ((horizontalLine 0)::Line 2 Float)
        ip4 = asA @(Point 2 Float) $ line_pi__ `intersect` ((horizontalLine 0)::Line 2 Float)
        ip5 = asA @(Point 2 Float) $ line_k_ `intersect` line_i_

        pk_ = pk .+^ ((signorm vvk)^*(fromIntegral boxSize))

        vvi 
            | si = Vector2 (vi^.yComponent) (-(vi^.xComponent))
            | otherwise = Vector2 (-(vi^.yComponent)) (vi^.xComponent)
        vvk 
            | sk = Vector2 (vk^.yComponent) (-(vk^.xComponent))
            | otherwise = Vector2 (-(vk^.yComponent)) (vk^.xComponent)

        dontIntersect Nothing = True --if l and k' do not intersect above the boundary then l does not block k
        dontIntersect (Just ip)
            | ip^.yCoord > 0 = False
            | otherwise = True


lengthFromIntersection :: Point 2 Float -> Maybe (Point 2 Float) -> Maybe Int
lengthFromIntersection _ Nothing     = Nothing --`debug` "no intersection"
lengthFromIntersection p (Just ip)
    | ip^.yCoord >= 0 = Just (ceiling (euclideanDist p ip)) --`debug` show ip
    | otherwise = Nothing --`debug` show ip

-- Gives a range of lengths for k where it will fit with l
doesFitLength :: [FSUnplacedLabel] -> Int -> Int -> Int -> [Int]
doesFitLength ls l len k = filter (fitLength_ ls l len k) lengths
    where lengths = [minLength (fst (ls!!k)) + e | e <- [0..64], e == 0 || e `mod` boxSize == 0] 
    
-- getLengths :: (Ix t, Eq a1, Num t, Num a1) => Array (t, t) (a2, (t, a1)) -> t -> t -> [a1]
getLengths r p1 p2
    | port == -1 = replicate (p2-p1 + 1) (-1)
    | (port,l)== (0,0) = []
    | otherwise = getLengths r p1 port ++ [l] ++ getLengths r port p2
    where (_,(port,l)) = r!(p1,p2) --`debug` show r

debug x y | debug_log = flip trace x y
        | otherwise = x
 
-- Determines if l' might fit with to l1
-- TODO its cb1 with cb is not relevant
fitBox :: [LineSegment 2 () Float] -> ((Int,(UnplacedLabel,Int)),(Int,(UnplacedLabel,Int)))-> Bool
-- fitBox ((_,(p1,i1)),(_,(p2,i2))) =  not (lb1 `intersects` lb) 
--     where
--         lb1 = leaderFromLabelLength_ (p1,i1) --`debug` ("i:" ++ show (leaderFromLabelLength (ls!!i) len1) ++ show len1)
--         lb = leaderFromLabelLength_ (p2,i2) --`debug` ("k:" ++ show (leaderFromLabelLength (ls!!k) len) ++ show len)
--         leaderFromLabelLength_ (ul@(_, c),i) = (ls, clueBoxPolygon (ls^.end.core) v s (length c))
--             where 
--                 ls = leader p v (minLength $ getPort (ul,i))
--                 Port p v s = getPort (ul,i)

fitBox lss ((_,(p1,i1)),(_,(p2,i2)))
    | isNothing ls = False
    | getPort (p1,i1) == getPort(p2,i2)  = True -- `debug` "true: positions are the same"
    | intersects l1 l = False  -- `debug` "false: i intersects k"
    | intersects b l1 = False  -- `debug` "false: box k intersects i"
    | intersects b1 l = False  -- `debug` "false: box k intersects i"
    | intersects b b1 = False
    | otherwise = True  -- `debug` "true: no intersection"
    where
        ls = getEdge (getPort (p1,i1)) lss
        l1 = (leader pos1 dir1 (minLengthEdge(getPort(p1,i1)) (fromJust ls))) -- `debug` ("i: " ++ show (leader pos1 dir1 boxSize))
        l = (leader pos dir (minLengthEdge(getPort(p2,i2)) (fromJust ls))) -- `debug` ("k: " ++ show (leader pos dir boxSize))
        b1 = clueBoxPolygon (l1^.end.core) dir1 s1 1
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

fitLength_ ls i len1 k len = not (lb1 `intersects` lb) --`debug` show (i,len1,k,len)
    where
        lb1 = leaderFromLabelLength (ls!!i) len1 --`debug` ("i:" ++ show (leaderFromLabelLength (ls!!i) len1) ++ show len1)
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

--determines the length of l2 is where it crosses l1
intersectionLength :: Port -> Port -> Float
intersectionLength p1 p2 = case ip of
    Just p -> euclideanDist (_location p1) p
    Nothing -> read "Infinity" :: Float
    where ip = asA (intersect (_line p1) (_line p2))

leaderFromLabelLength :: FSUnplacedLabel -> Int -> Leader
leaderFromLabelLength (Port p v s, c) i = (ls, clueBoxPolygon (ls^.end.core) v s (length c))
    where ls = leader p v i --`debug` show (p,v,i)

--Cluebox on the boundary
clueBoxFromLabel :: FSUnplacedLabel -> ClueBox
clueBoxFromLabel (Port p v s,c) = clueBoxPolygon p v s (length c)

--Returns the edge which p is on
getEdge :: Port -> [LineSegment 2 () Float] -> Maybe (LineSegment 2 () Float)
getEdge (Port pos _ _) ls = getEdge_ $ filter (\x -> x `isOnEdge` pos) ls
    where
        getEdge_ [] = Nothing
        getEdge_ ls = Just (head ls)  

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

