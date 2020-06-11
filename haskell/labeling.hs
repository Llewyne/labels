{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Labeling where

--import Graphics.Gloss
import Data.Array
import Data.List hiding (intersect)
import Debug.Trace
import Data.Ord
import Data.Vinyl.CoRec


import Control.Lens

import Data.Geometry hiding (head)
import Data.Geometry.PlanarSubdivision
--import Data.Geometry.Ipe


data CAS = CAS

type Frame    = SimplePolygon () Float
type Interior = PlanarSubdivision CAS () () Bool Float
type Clue     = [Int]

data Port = Port 
  { _location  :: Data.Geometry.Point 2 Float
  , _direction :: Data.Geometry.Vector 2 Float
  , _side      :: Bool
  } deriving (Show,Eq)
$(makeLenses ''Port)

data Label = Label
  { _clue      :: Clue
  , _port      :: Port
  , _offset    :: Float
  } deriving (Show,Eq)
$(makeLenses ''Label)

data Nonogram = Nonogram
  { _frame     :: Frame
  , _interior  :: Interior
  , _labels    :: [Label]
  }
$(makeLenses ''Nonogram)

type UnplacedLabel = ([Port], Clue)

_line :: Port -> Line 2 Float
_line p = Data.Geometry.Line (_location p) (Labeling._direction p)


-- parseLeader :: Label -> [Picture]
-- parseLeader l = leader (l.port.location) (l.clue) (l.offset) (l.port.direction) (l.port.side)



placeLabelsDynamic :: Int        --index of the first port
    -> Float
    -> Int              --index of the second port
    -> Float
    -> [UnplacedLabel]
    -> [Label]
placeLabelsDynamic l1 e1 l2 e2 ls
    | l1 > l2       = []
    | l1 == l2      = []
    | l1 + 1 == l2 && 
        (intersectionLength p1 p2 >= (e2 + fromIntegral (length (snd (ls!!(l2-1)))) + 1) 
        || intersectionLength p2 p1 >= (e1 + fromIntegral (length (snd (ls!!(l1-1)))) + 1))
            = [Label (snd (ls!!l1)) p1 e1,Label (snd (ls!!l2)) p2 e2]
    | otherwise     =  nub (left splitAt ++ right splitAt)
        where
            p1 = (head.fst) (ls!!l1)
            p2 = (head.fst) (ls!!l2)
            maxLength l 
                | e1 <= e2 = min (min (intersectionLength p1 ((head.fst) (ls!!l))) e1) e2
                | otherwise = min (min (intersectionLength p2 ((head.fst) (ls!!l))) e1) e2
            splitAt
                | l1+2==l2  = (l1+1,1)
                | otherwise = maximumBy (\y->comparing snd y) ((0,0): filter (\(x,xl)-> (length (left (x,xl)) > 0) && (length (right (x,xl)) > 0)) [(l,el) | l<-[(l1+1)..(l2-1)],el <- [maxLength l]]) 
            left (0,0) = []
            left (x,xl) =  (placeLabelsDynamic l1 e1 x xl ls)
            right (0,0) = []
            right (x,xl) = (placeLabelsDynamic x xl l2 e2 ls)

--determines the length of l2 is where it crosses l1
intersectionLength :: Port -> Port -> Float
intersectionLength p1 p2 = case ip of
    Just p -> euclideanDist (_location p1) p
    Nothing -> read "Infinity" :: Float
    where ip = asA (intersect (_line p1) (_line p2))

-- drawResult p e n ps r
--     = display
--          (InWindow
--            "Labels"      -- window title
--         (floor width, floor height)      -- window size
--         (100, 100))      -- window position
--     white            -- background color
--     (results n ps r 100.0 400.0)     -- picture to display

-- result p1 e1 n1 p2 e2 n2 ps =  Pictures (concat [(leader (100,(pos1)) n1 (e1) (slopeToDegrees m1) (sideToNum s1)),(leader (100,(pos2)) n2 (e2) (slopeToDegrees m2) (sideToNum s2))])
--     where
--         (Port s1 pos1 m1) = (ps!p1)
--         (Port s2 pos2 m2) = (ps!p2)

-- results :: [[Int]] -> Array Int Port -> [(Int,Float)] -> Float -> Float-> Picture
-- results n ps rs w h = Pictures   ((concat (map (\x->result1 (ps!(fst x)) w h (snd x) (n!!(fromIntegral ((fst x)-1)))) rs)) ++ ( nonogram [] [] w h) (map getSlope (elems ps)) (map getPos (elems ps)))

-- getSlope (Port b s pos m) = -m

-- getPos (Port b s pos m) = pos*20

-- result1 :: Port -> Float ->  Float -> Float -> [Int] -> [Picture]
-- result1 (Port Labeling.Right s pos m) w h e n = leader (100+w/20,(pos)) n (e) (slopeToDegrees m) (sideToNum s)
-- result1 (Port Labeling.Left s pos m) w h e n = leader (100,(pos)) n (-e) (slopeToDegrees m) (sideToNum s)
-- result1 (Port Labeling.Top s pos m) w h e n = leader (100+w,(pos)) n (e) (slopeToDegrees m) (sideToNum s)
-- result1 (Port Labeling.Bottom s pos m) w h e n = leader (100+w,(pos)) n (e) (slopeToDegrees m) (sideToNum s)