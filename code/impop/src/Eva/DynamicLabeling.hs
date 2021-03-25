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
import Eva.ClueBox hiding (boxSize)
import Eva.Test
import SAT.Mios (CNFDescription (..), solveSAT)

type FSUnplacedLabel = (Port, Clue) --Unplaced Label with a fixed side

boxSize = 16
unit = 3


-- places labels dynamically
placeLabelsDynamic :: [UnplacedLabel] -> Frame -> IO [Label] 
placeLabelsDynamic ls f = do
    ls_ <- assignPorts ls
    return $ concatMap (placeLabelsDynamicEdge ls_) (simplify $ listEdges f)
    

assignPorts :: [UnplacedLabel] -> IO [FSUnplacedLabel]
assignPorts ul = do
    let ml = mapVars ul
    -- mapM_ (putStrLn . show) ml
    let clauses = setClauses ml
    -- putStrLn $ show clauses
    let desc = CNFDescription (length $ concat ml) (length clauses) ""
    asg <- solveSAT desc clauses
    -- putStrLn $ show asg
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


differentPort :: ((Int,(UnplacedLabel,Int)),(Int,(UnplacedLabel,Int)))-> Bool
differentPort ((_,l1),(_,l2)) = (_location $ getPort l1) /= (_location $ getPort l2)

getPort :: (UnplacedLabel,Int) -> Port
getPort ((ps,_),i) = ps!!(i-1)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

-- map the variables for the SAT clauses, result is a list of pairs where the first element is the index of the variable and the second element the port assignment
mapVars :: [UnplacedLabel] -> [[(Int,(UnplacedLabel,Int))]]
mapVars ul = _mapVars (map getVar ul) 1

_mapVars :: [[(UnplacedLabel,Int)]] -> Int -> [[(Int,(UnplacedLabel,Int))]]
_mapVars [] _ = []
_mapVars (v:vs) i = zip [i..] v : _mapVars vs (i + (length v))

getVar :: UnplacedLabel -> [(UnplacedLabel,Int)]
getVar ul = [(a,b)|a<-[ul],b<-[1..(length (fst ul))] ]

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
                        m = [(fst (f i k a c) + fst (f k j c b) - c,(k,c)) | k<-[i+1..j-1], c<-set k , valid i a j b k c ] `debug` (show i ++ show j)
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
 
-- Determines if l' might fit with to l1
fitBox :: ((Int,(UnplacedLabel,Int)),(Int,(UnplacedLabel,Int)))-> Bool
fitBox ((_,(p1,i1)),(_,(p2,i2))) 
    | pos1 == pos || pos2 == pos = True -- `debug` "true: positions are the same"
    | intersects l1 l = False -- `debug` "false: i intersects k"
    | intersects b l1 = False -- `debug` "false: box k intersects i"
    | otherwise = True -- `debug` "true: no intersection"
    where
        l1 = (leader pos1 dir1 boxSize) --`debug` ("i: " ++ show (leader pos1 dir1 len1))
        l = (leader pos dir boxSize) --`debug` ("k: " ++ show (leader pos dir len))
        b = clueBoxPolygon (l^.end.core) dir s
        Port pos1 dir1 s1 = getPort (p1,i1)
        Port pos dir s = getPort (p2,i2)

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

-- initialize leader lengths
initLeaders l = (maxLength:leader1:take (l-2) (repeat 0))++ [leaderN,maxLength]
    where maxLength = l^2 -- maxLength of leader

-- Makes dummies, assumes line segment is horizontal
dummy0 :: LineSegment 2 () Float -> FSUnplacedLabel
dummy0 s = (Port (Point2 ((s^.start.core.xCoord) - unit) (s^.start.core.yCoord)) (Vector2 (unit) unit) True,[0])
dummyNplus1 :: LineSegment 2 () Float -> FSUnplacedLabel
dummyNplus1 s = (Port (Point2 ((s^.end.core.xCoord) - unit) (s^.end.core.yCoord)) (Vector2 (-unit) unit) False,[0])

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