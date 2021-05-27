{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Eva.ClueBox where

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

import Debug.Trace

debug_log = False


debug x y | debug_log = flip trace x y
        | otherwise = x

--------------------------------------------------------------------------------
-- Clue box type: rectangle that is not orthogonal
type ClueBox = SimplePolygon () Float

type instance IntersectionOf (ClueBox) (LineSegment 2 () Float) = [ NoIntersection, [Point 2 Float], LineSegment 2 () Float]

instance (ClueBox) `IsIntersectableWith` (LineSegment 2 () Float) where
    nonEmptyIntersection = defaultNonEmptyIntersection

    cb `intersect` ls =
     case first List.nub . partitionEithers . mapMaybe collect $ sides of
       ([],_)   -> coRec NoIntersection
       (a,_)    -> coRec a
       (a,b)     -> error $ "intersecting a line with a box. Box is degenerate" ++ show a ++ show b
     where
       sides = listEdges cb

       collect   :: LineSegment 2 () Float -> Maybe (Either (Point 2 Float) (LineSegment 2 () Float))
       collect s = match (s `intersect` ls) $
                        (H $ \NoIntersection           -> Nothing)
                     :& (H $ \(a :: Point 2 Float)         -> Just $ Left a)
                     :& (H $ \(e :: LineSegment 2 () Float) -> Just $ Right e)
                     :& RNil

type instance IntersectionOf (ClueBox) ClueBox = [ NoIntersection, [Point 2 Float], [LineSegment 2 () Float]]

instance ClueBox `IsIntersectableWith` ClueBox where
    nonEmptyIntersection = defaultNonEmptyIntersection

    cb `intersect` cb_ =  
     case first (List.nub . concat) . partitionEithers . mapMaybe collect $ sides  of
       ([],[])      -> coRec NoIntersection
       ([],[s1,s2]:_)  -> coRec [s1,s2]
       ([],_)       -> coRec NoIntersection
       ([a],[])     -> coRec NoIntersection
       (a,_)        -> coRec a
       (a,b)     -> error $ "intersecting a line with a box. Box is degenerate" ++ show a ++ show b
     where
       sides = listEdges cb

       collect   :: LineSegment 2 () Float -> Maybe (Either [Point 2 Float] [LineSegment 2 () Float])
       collect s = match (cb_ `intersect_` s) $
                        (H $ \NoIntersection           -> Nothing)
                     :& (H $ \(a :: [Point 2 Float])        -> Just $ Left a)
                     :& (H $ \(e :: [LineSegment 2 () Float]) -> Just $ Right e)
                     :& RNil
       cb_ `intersect_` ls =
        case first List.nub . partitionEithers . mapMaybe collect $ sides of --`debug` show (partitionEithers . mapMaybe collect $ sides) of
            ([],[]) -> coRec NoIntersection
            (a,[])  -> coRec a
            (_,s)   -> coRec s

            -- ([], [s1,s2,s3])  -> coRec NoIntersection -- $ first (const ()) s    Technically intersects but three line segment is ok
            (a,b)     -> error $ "intersection not recognized" ++ show a ++ show b
            where
                sides = listEdges cb_

                collect   :: LineSegment 2 () Float -> Maybe (Either (Point 2 Float) (LineSegment 2 () Float))
                collect s = match (s `intersect` ls) $
                                    (H $ \NoIntersection           -> Nothing)
                                :& (H $ \(a :: Point 2 Float)         -> Just $ Left a)
                                :& (H $ \(e :: LineSegment 2 () Float) -> Just $ Right e)
                                :& RNil

--------------------------------------------------------------------------

boxSize = 16

-- Create a clue box for line l
clueBoxPolygon 
    :: Point 2 Float    -- Attachment point
    -> Vector 2 Float   -- Direction of l
    -> Bool             -- Side that must be labeled. True if on right of direction vector (left if going into puzzle)
    -> Int              -- How many boxes
    -> ClueBox
clueBoxPolygon p v False i = fromPoints [p :+ (),p2 :+ (),p3 :+ (),p4 :+ ()] --`debug` (show (p,p2,p3,p4))
    where
        ub = signorm v^*(fromIntegral boxSize)
        iv = inverseVector ub
        p2 = p .+^ ub^*(fromIntegral i)
        p3 = p2 .+^ (Vector2 (-iv^.xComponent) (iv^.yComponent))
        p4 = p3 .+^ (negated ub^*(fromIntegral i))
clueBoxPolygon p v True i = fromPoints [p :+ (),p2 :+ (),p3 :+ (),p4 :+ ()] --`debug` (show (p,p2,p3,p4))
    where
        ub = signorm v^*(fromIntegral boxSize)
        iv = inverseVector ub
        p2 =  p .+^ ub^*(fromIntegral i)
        p3 = p2 .+^ (Vector2 (iv^.xComponent) (-iv^.yComponent))
        p4 = p3 .+^ (negated ub^*(fromIntegral i))

