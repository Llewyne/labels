module Labeling where

import Drawing
import Data.Array
import Data.List
import Debug.Trace

--Problem
data Side = Top | Bottom | Left | Right deriving (Eq,Show)
type Polygon = [Side]
data Port = Port Side Float Float deriving Show -- side position slope
data Line = Line Port Port Integer Integer

--Solution
data Label = Label {l::Integer}
type Labeling = [(Label,Port,Integer)] --Label,Port and extension length

p1 = Port Labeling.Right 1 (-0.1)
p2 = Port Labeling.Right 3 1
p3 = Port Labeling.Right 5 (0.1)
ps = listArray (1,3) [p1,p2,p3]

test1 = labelable (1,3) (3,1) ps

-- (port number, extent) (port number, extent) list of ports
labelable :: (Integer,Integer)->(Integer,Integer)->Array Integer Port->Array ((Integer,Integer),(Integer,Integer)) Bool
labelable (p1,e1) (p2,e2) ps = r!(p1,p2)
            where
                r = array ((p1,p1),(p2,p2)) [((i,j),(f (i,j)))|i<-[p1..p2],j<-[p1..p2]]
                eMax = e1 + e2
                f (i,j) = array (((i,i),(1,1)),((j,j),(eMax,eMax)))  (map g [(a,b,c,d)|a<-[i..j],b<-[i..j],c<-[1..eMax],d<-[1..eMax]]) -- for each pair of ports, a list of possible lengths and validity
                    where
                        g (i,j,ei,ej) -- if length ei for port i and length ej for port j are a valid pair
                            | i == j = (((i,j),(ei,ej)),True)
                            | i > j = (((i,j),(ei,ej)),False)
                            | j-i == 1 = (((i,j),(ei,ej)),k) -- if they are neighbouring ports
                            | otherwise = (((i,j),(ei,ej)),m)
                            where
                                l =  floor((fromIntegral(i+j))/2) -- port in the middle of i and j
                                Port _ posi mi = ps!i
                                Port _ posj mj = ps!j
                                Port _ posl ml = ps!l
                                h i j l ei ej el = r!(i,l)!((i,l),(ei,el)) && r!(l,j)!((l,j),(el,ej)) && (fitLength (ps!i) ei (ps!j) ej (ps!l) el)
                                k = not (leaderIntersect posi mi posj mj (ei,ej))
                                m = or (map (h i j l ei ej) [1..eMax])
                                        

--otherwise = and (concat (zipWith map (cycle (map (fitLength (ps!p1) e1 (ps!p2) e2) (map (ps!) [(p1+1)..(p2-1)]))) (permutations [1..(e1+e2)])))

-- shorthand for fitlength, index of first port, index of second port, index of fitting port, length first port, length second port, length fitting port, all ports
h i j k ei ej ek ps = (fitLength (ps!i) ei (ps!j) ej (ps!k) ek)

findFits :: Port -> Integer -> Port -> Integer -> Port -> [Integer]
findFits p1 len1 p2 len2 p = filter (fitLength p1 len1 p2 len2 p) [1..len1+len2]

-- determines if a leader with length len fits between two ports with lengths len1 and len2
fitLength :: Port -> Integer -> Port -> Integer -> Port -> Integer -> Bool
fitLength (Port s1 pos1 m1) len1 (Port s2 pos2 m2) len2 (Port s pos m) len 
    | s1 /= s2 = True
    | leaderIntersect pos1 m1 pos m (fromIntegral len1,fromIntegral len) = False
    | leaderIntersect pos m pos2 m2 (fromIntegral len,fromIntegral len2) = False
    | otherwise = True


-- Do two leaders, defined by their y-coordinate position, slope and length, intersect
leaderIntersect :: Float -> Float -> Float-> Float -> (Integer,Integer) -> Bool
leaderIntersect y1 m1 y2 m2 (len1,len2) 
    | m1 == m2 = False
    | px <= 0 || (px > (fromIntegral len1) && px > (fromIntegral len2)) = False
    | otherwise = True
    where (px,_) = lineIntersection m1 y1 m2 y2

-- intersection point = ((y2 - y1)/(m1-m2),(m1*y2-m2*y1)/(m1-m2)
lineIntersection :: Float -> Float -> Float -> Float -> (Float,Float)
lineIntersection m1 b1 m2 b2 = ((b2-b1)/(m1-m2),(m1*b1-m2*b2)/(m1-m2))



