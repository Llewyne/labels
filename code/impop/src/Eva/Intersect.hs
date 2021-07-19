{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Eva.Intersect where

import Data.List hiding (intersect)
import qualified Data.List (intersect)
import Data.List.Split
import Data.Ord
import Data.Vinyl hiding (Label)
import Data.Vinyl.CoRec
import Data.Ext
import Data.Maybe

import Control.Lens

import Data.Geometry hiding (head)

import Nonogram

import Eva.Leader hiding (boxSize)
import Eva.ClueBox
import Eva.NonoUtil
import Eva.Util

import Debug.Trace


type FSUnplacedLabel = (Port, Clue) --Unplaced Label with a fixed side


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

-- Determines if l' might fit with to l1
-- TODO its cb1 with cb is not relevant
fitBox :: [LineSegment 2 () Float] -> ((Int,(UnplacedLabel,Int)),(Int,(UnplacedLabel,Int)))-> Bool
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


leaderFromLabelLength :: FSUnplacedLabel -> Int -> Leader
leaderFromLabelLength (Port p v s, c) i = (ls, clueBoxPolygon (ls^.end.core) v s (length c))
    where ls = leader p v i --`debug` show (p,v,i)

leaderFromPortCluesLength :: Port -> [Int] -> Int -> Leader
leaderFromPortCluesLength p c i = leaderFromLabelLength (p,c) i