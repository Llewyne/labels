{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Labeling where

import Graphics.Gloss
import Data.Array
import Data.List hiding (intersect)
import Data.List.Split
import Debug.Trace
import Data.Ord
import Data.Vinyl.CoRec

import Drawing
import Tests


import Control.Lens

import Data.Geometry hiding (head,direction,init)
import Data.Geometry.PlanarSubdivision hiding(location)
--import Data.Geometry.Ipe


data CAS = CAS --empty data type referentie naar arrangement

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

--Test
p1 = Port (Point2 0 0) (Vector2 1 (1)) False
p2 = Port (Point2 0 100) (Vector2 0 1) False
p3 = Port (Point2 0 175) (Vector2 (-1) 1) False
p4 = Port (Point2 0 250) (Vector2 0 1) False
p5 = Port (Point2 0 325) (Vector2 1 (-1)) False

test1 = [([p1],[1,2]::[Int]),([p2],[2,3]::[Int]),([p3],[1]::[Int]),([p4],[1]::[Int]),([p5],[1]::[Int])]

runsidetest1 = drawResult 0 1 8 1 (take 9 (map parseLabel sidetest1))

drawResult l1 e1 l2 e2 ls
    = display
         (InWindow
           "Labels"      -- window title
        (floor width, floor height)      -- window size
        (100, 100))      -- window position
    white            -- background color
    (labelsToPicture (placeLabelsDynamic l1 e1 l2 e2 ls))     -- picture to display

parseLabel :: String -> UnplacedLabel
parseLabel s = ([(Port (Point2 0 (height-y)) (parseVector angle) (side /= 1))], nums)
    where 
        ws = words s
        y = read (ws!!0)
        angle = read (ws!!1)
        side = read (ws!!3)
        nums = map read (splitOn "," (init (tail (ws!!2))))

parseVector 0   = Vector2 0 0 
parseVector (-45)  = Vector2 1 1
parseVector (45) = Vector2 1 (-1)
parseVector _ = Vector2 0 0 

--nonogramToPicture :: Nonogram -> Picture
--nonogramToPicture n = Drawing.nonogram 

labelsToPicture :: [Label] -> Picture
labelsToPicture ls = Pictures (map labelToPicture ls)


labelToPicture :: Label -> Picture
labelToPicture (Label c (Port (Point2 x y) (Vector2 vx vy) s) o)  = Pictures (leader (x,y) c o (vx/vy) (fromEnum s))

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
        (intersectionLength p1 p2 >= (e2 + (fromIntegral (length (snd (ls!!l2))))*boxSize + 1) 
        || intersectionLength p2 p1 >= (e1 + (fromIntegral (length (snd (ls!!l1))))*boxSize + 1))
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