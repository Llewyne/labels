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

p1 = Port Labeling.Right 1 0.5
p2 = Port Labeling.Right 3 1
p3 = Port Labeling.Right 5 (-0.5)
ps = listArray (1,3) [p1,p2,p3]

-- (port number, extent) (port number, extent) list of ports
labelable :: (Integer,Integer)->(Integer,Integer)->Array Integer Port->IO()
labelable (p1,e1) (p2,e2) ps = 
    do 
        let result = r!(p1,p2)
        print result
            where
                r = array ((p1,p1),(p2,p2)) [((i,j),(f (i,j)))|i<-[p1..p2],j<-[p1..p2]]
                eMax = e1 + e2
                f (i,j) = listArray ((1,1),(eMax,eMax)) (map g [(a,b,c,d)|a<-[i],b<-[j],c<-[1..eMax],d<-[1..eMax]])
                    where
                        g (i,j,ei,ej)
                            | i == j = True
                            | i > j = False
                            | j-i == 1 = not (leaderIntersect posi mi posj mj (ei,ej))
                            | otherwise = or (map (h i j l ei ej) [1..eMax]) 
                            where
                                l =  floor((fromIntegral(i+j))/2)
                                Port _ posi mi = ps!i
                                Port _ posj mj = ps!j
                                Port _ posl ml = ps!l
                                h i j l ei ej el = r!(i,l)!(ei,el) && r!(l,j)!(el,ej) && (fitLength (ps!i) ei (ps!j) ej (ps!l) el)
                                        

--otherwise = and (concat (zipWith map (cycle (map (fitLength (ps!p1) e1 (ps!p2) e2) (map (ps!) [(p1+1)..(p2-1)]))) (permutations [1..(e1+e2)])))

h i j k ei ej ek ps = (fitLength (ps!i) ei (ps!j) ej (ps!k) ek)

findFits :: Port -> Integer -> Port -> Integer -> Port -> [Integer]
findFits p1 len1 p2 len2 p = filter (fitLength p1 len1 p2 len2 p) [1..len1+len2]

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



