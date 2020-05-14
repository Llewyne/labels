module Labeling where

import Graphics.Gloss
import Drawing
import Data.Array
import Data.List
import Debug.Trace
import Data.Ord

--Problem
data Side = Above | Below deriving (Eq,Show)
data Border = Left | Top | Right | Bottom deriving (Eq,Show)
type Polygon = [Border]
data Port = Port Border Side Float Float deriving Show -- side position slope
data Line = Line Port Port Int Int

--Solution
data Label = Label {l::Int}
type Labeling = [(Label,Port,Int)] --Label,Port and extension length

p1 = Port Labeling.Right Labeling.Below 0 (1)
p2 = Port Labeling.Right Labeling.Below 4 0
p3 = Port Labeling.Right Labeling.Below 8 (1)
p4 = Port Labeling.Right Labeling.Below 12 0
p5 = Port Labeling.Right Labeling.Below 16 (-1)
ps = listArray (1,5) [p1,p2,p3,p4,p5]
ps2 = listArray (1,2) [p1,p3]

test1 = labelable 1 100 3 100 ps

test2 = showResult [1,3] [3,4] [[1],[2]] ps

test3 = showResult [1,3] [1,2] [[1],[2]] ps

test4 = showResult [1,2] [2,1] [[1],[2]] ps2

test5 = showResult [1,2] [2,0.5] [[1],[2]] ps2

test6 = showResult [1,2] [1.5,0.5] [[1],[2]] ps2

test7 = showResult [1,5] [1,1] [[1],[2],[3],[4],[5]] ps

showResult p e n ps = do 
    print r
    drawResult p e n ps r
        where
            r = labelable1 (head p) (head e) (last p) (last e) ps (map length n)

drawResult p e n ps r
    = display
         (InWindow
           "Labels"      -- window title
        (floor width, floor height)      -- window size
        (100, 100))      -- window position
    white            -- background color
    (results n ps r 100.0 400.0)     -- picture to display

-- result p1 e1 n1 p2 e2 n2 ps =  Pictures (concat [(leader (100,(pos1)) n1 (e1) (slopeToDegrees m1) (sideToNum s1)),(leader (100,(pos2)) n2 (e2) (slopeToDegrees m2) (sideToNum s2))])
--     where
--         (Port s1 pos1 m1) = (ps!p1)
--         (Port s2 pos2 m2) = (ps!p2)

results :: [[Int]] -> Array Int Port -> [(Int,Float)] -> Float -> Float-> Picture
results n ps rs w h = Pictures   ((concat (map (\x->result1 (ps!(fst x)) w h (snd x) (n!!(fromIntegral ((fst x)-1)))) rs)) ++ ( nonogram [] [] w h) (map getSlope (elems ps)) (map getPos (elems ps)))

getSlope (Port b s pos m) = -m

getPos (Port b s pos m) = pos*20

result1 :: Port -> Float ->  Float -> Float -> [Int] -> [Picture]
result1 (Port Labeling.Right s pos m) w h e n = leader (100+w/20,(pos)) n (e) (slopeToDegrees m) (sideToNum s)
result1 (Port Labeling.Left s pos m) w h e n = leader (100,(pos)) n (-e) (slopeToDegrees m) (sideToNum s)
result1 (Port Labeling.Top s pos m) w h e n = leader (100+w,(pos)) n (e) (slopeToDegrees m) (sideToNum s)
result1 (Port Labeling.Bottom s pos m) w h e n = leader (100+w,(pos)) n (e) (slopeToDegrees m) (sideToNum s)

labelable :: Int        --index of the first port
    -> Float              --length of the first leader
    -> Int              --index of the second port
    -> Float              --length of the second leader
    -> Array Int Port   --array of all ports
    -> [Int]                --list of amount of boxes
    -> Bool

labelable p1 e1 p2 e2 ps ns
    | p1 > p2       = False
    | p1 == p2      = True
    | p1 + 1 == p2  = intersectionLength (ps!p1) (ps!p2) >= (e2 + fromIntegral (ns!!(p2-1)) +  fromIntegral (fromEnum (s1 == Labeling.Above))) || intersectionLength (ps!p2) (ps!p1) >= (e1 + fromIntegral (ns!!(p1-1)) + fromIntegral (fromEnum (s2 == Labeling.Below)))
    | otherwise     = or [l > 0 && (labelable p1 e1 l el ps ns) && (labelable l el p2 e2 ps ns)|l<-[(p1+1)..(p2-1)],el <- [maxLength l]]
        where
            maxLength l 
                | e1 <= e2 = intersectionLength (ps!p1) (ps!l)
                | otherwise = intersectionLength (ps!p2) (ps!l)
            (Port _ s1 _ _) = (ps!p1)
            (Port _ s2 _ _) = (ps!p2)

labelable1 :: Int        --index of the first port
    -> Float              --length of the first leader
    -> Int              --index of the second port
    -> Float              --length of the second leader
    -> Array Int Port   --array of all ports
    -> [Int]
    -> [(Int,Float)]

labelable1 p1 e1 p2 e2 ps ns
    | p1 > p2       = []
    | p1 == p2      = []
    | p1 + 1 == p2 && 
        (intersectionLength (ps!p1) (ps!p2) >= (e2 + fromIntegral (ns!!(p2-1)) + fromIntegral (fromEnum (s1 == Labeling.Above))) 
        || intersectionLength (ps!p2) (ps!p1) >= (e1 + fromIntegral (ns!!(p1-1)) + fromIntegral (fromEnum (s2 == Labeling.Below)))) 
            = [(p1,e1),(p2,e2)]
    | otherwise     =  nub (left splitAt ++ right splitAt)
        where
            maxLength l 
                | e1 <= e2 = min (min (intersectionLength (ps!p1) (ps!l)) e1) e2
                | otherwise = min (min (intersectionLength (ps!p2) (ps!l)) e1) e2
            (Port _ s1 _ _) = (ps!p1)
            (Port _ s2 _ _) = (ps!p2)
            splitAt
                | p1+2==p2  = (p1+1,1)
                | otherwise = maximumBy (\y->comparing snd y) ((0,0): filter (\(x,xl)-> (length (left (x,xl)) > 0) && (length (right (x,xl)) > 0)) [(l,el) | l<-[(p1+1)..(p2-1)],el <- [maxLength l]]) 
            left (0,0) = []
            left (x,xl) =  (labelable1 p1 e1 x xl ps ns)
            right (0,0) = []
            right (x,xl) = (labelable1 x xl p2 e2 ps ns)

-- (port number, extent) (port number, extent) list of ports
-- labelable :: (Int,Int)->(Int,Int)->Array Int Port->Array ((Int,Int),(Int,Int)) Bool
-- labelable (p1,e1) (p2,e2) ps = r!(p1,p2)
--             where
--                 r = array ((p1,p1),(p2,p2)) [((i,j),(f (i,j)))|i<-[p1..p2],j<-[p1..p2]]
--                 eMax = e1 + e2
--                 f (i,j) = array (((i,i),(1,1)),((j,j),(eMax,eMax)))  (map g [(a,b,c,d)|a<-[i..j],b<-[i..j],c<-[1..eMax],d<-[1..eMax]]) -- for each pair of ports, a list of possible lengths and validity
--                     where
--                         g (i,j,ei,ej) -- if length ei for port i and length ej for port j are a valid pair
--                             | i == j = (((i,j),(ei,ej)),True)
--                             | i > j = (((i,j),(ei,ej)),False)
--                             | j-i == 1 = (((i,j),(ei,ej)),k) -- if they are neighbouring ports
--                             | otherwise = (((i,j),(ei,ej)),m)
--                             where
--                                 l =  floor((fromIntegral(i+j))/2) -- port in the middle of i and j
--                                 Port _ posi mi = ps!i
--                                 Port _ posj mj = ps!j
--                                 Port _ posl ml = ps!l
--                                 h i j l ei ej el = r!(i,l)!((i,l),(ei,el)) && r!(l,j)!((l,j),(el,ej)) && (fitLength (ps!i) ei (ps!j) ej (ps!l) el)
--                                 k = not (leaderIntersect posi mi posj mj (ei,ej))
--                                 m = or (map (h i j l ei ej) [1..eMax])
                                        

--otherwise = and (concat (zipWith map (cycle (map (fitLength (ps!p1) e1 (ps!p2) e2) (map (ps!) [(p1+1)..(p2-1)]))) (permutations [1..(e1+e2)])))

-- shorthand for fitlength, index of first port, index of second port, index of fitting port, length first port, length second port, length fitting port, all ports
h i j k ei ej ek ps = (fitLength (ps!i) ei (ps!j) ej (ps!k) ek)

findFits :: Port -> Int -> Port -> Int -> Port -> [Int]
findFits p1 len1 p2 len2 p = filter (fitLength p1 len1 p2 len2 p) [1..len1+len2]

-- determines if a leader with length len fits between two ports with lengths len1 and len2
fitLength :: Port -> Int -> Port -> Int -> Port -> Int -> Bool
fitLength (Port _ s1 pos1 m1) len1 (Port _ s2 pos2 m2) len2 (Port _ s pos m) len 
    | s1 /= s2 = True
    | leaderIntersect pos1 m1 pos m (fromIntegral len1,fromIntegral len) = False
    | leaderIntersect pos m pos2 m2 (fromIntegral len,fromIntegral len2) = False
    | otherwise = True


-- Do two leaders, defined by their y-coordinate position, slope and length, intersect
leaderIntersect :: Float -> Float -> Float-> Float -> (Int,Int) -> Bool
leaderIntersect y1 m1 y2 m2 (len1,len2) 
    | m1 == m2 = False
    | px <= 0 || (px > (fromIntegral len1) && px > (fromIntegral len2)) = False
    | otherwise = True
    where (px,_) = lineIntersection m1 y1 m2 y2

-- intersection point = ((y2 - y1)/(m1-m2),(m1*y2-m2*y1)/(m1-m2)
lineIntersection :: Float -> Float -> Float -> Float -> (Float,Float)
lineIntersection m1 b1 m2 b2 = ((b2-b1)/(m1-m2),(m1*b1-m2*b2)/(m1-m2))

-- determines the length of l2 is where it crosses l1
intersectionLength :: Port -> Port -> Float
intersectionLength (Port _ _ pos1 m1) (Port _ _ pos2 m2) 
    | 0 < (snd t) = distance (0,pos2) t
    | otherwise = read "Infinity" :: Float
    where t = (lineIntersection m1 pos1 m2 pos2)

distance :: (Float,Float) -> (Float,Float) -> Float
distance (a,b) (c,d) = sqrt ((a-c)**2 + (b-d)**2)

sideToNum :: Side -> Int
sideToNum s 
    | s == Labeling.Above = 1
    | otherwise = 0


slopeToDegrees :: Float -> Float
slopeToDegrees m = (atan m)*180/pi