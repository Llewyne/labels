module Eva.NonoUtil where

import Eva.Util

    
import Nonogram

import Data.Geometry hiding (head)

import Data.Maybe

--Returns the edge which p is on
getEdge :: Port -> [LineSegment 2 () Float] -> Maybe (LineSegment 2 () Float)
getEdge (Port pos _ _) ls = getEdge_ $ filter (\x -> x `isOnEdge` pos) ls
    where
        getEdge_ [] = Nothing
        getEdge_ ls = Just (head ls)  

--Shorthand for getting the ith possible port from an unplaced label
getPort :: (UnplacedLabel,Int) -> Port
getPort ((ps,_),i) = ps!!(i-1)