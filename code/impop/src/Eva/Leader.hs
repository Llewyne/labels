{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Eva.Leader where

import           Control.Lens
import           Data.Vinyl hiding (Label)
import           Data.Vinyl.CoRec
import           Data.Ext
import           Data.Geometry hiding (head,direction,init,replicate,unit)
import           Data.Geometry.PlanarSubdivision hiding(location)
import           Data.Geometry.Polygon
import qualified Data.List as List
import           Data.Bifunctor
import           Data.Either (partitionEithers)
import           Data.Maybe (mapMaybe)
import           Eva.Util
import           Eva.ClueBox hiding (boxSize,debug_log,debug_log)

import Debug.Trace

debug_log = False


debug x y | debug_log = flip trace x y
        | otherwise = x

--------------------------------------------------------------------------------
-- Leader type: A full label, a linesegment and (one or more) clueboxes
type Leader = ClueBox   -- The same intersection rules apply so it is basically a cluebox


--------------------------------------------------------------------------

boxSize = 16

-- Create a leader from an FSUnplacedLabel and a length
leaderPolygon :: FSUnplacedLabel -> Int -> Leader
leaderPolygon (Port p v False,c) len = fromPoints [p :+ (),p2 :+ (),p3 :+ (),p4 :+ (),p5 :+ ()] -- Lefty
    where
        ub = signorm v^*(fromIntegral boxSize)
        iv = inverseVector ub
        p1 = p .+^ ((signorm v)^*(fromIntegral len)+ boxSize*length c)
        p2 = p1 .+^ ub
        p3 = p2 .+^ (Vector2 (-iv^.xComponent) (iv^.yComponent))
        p4 = p3 .+^ (negated ub)

leaderPolygon (Port p v True,c) len = fromPoints [p :+ (),p2 :+ (),p3 :+ (),p4 :+ (),p5 :+ ()] -- Lefty
    where
        ub = signorm v^*(fromIntegral boxSize)
        iv = inverseVector ub
        p1 = p .+^ ((signorm v)^*(fromIntegral len)+ boxSize*length c)
        p2 = p1 .+^ ub
        p3 = p2 .+^ (Vector2 (iv^.xComponent) (-iv^.yComponent))
        p4 = p3 .+^ (negated ub)