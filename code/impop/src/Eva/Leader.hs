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
import           Data.Maybe (catMaybes)
import           Eva.Util
import           Eva.ClueBox hiding (boxSize,debug_log,debug_log)

import Debug.Trace

debug_log = False


debug x y | debug_log = flip trace x y
        | otherwise = x

--------------------------------------------------------------------------------
-- Leader type: A full label, a linesegment and (one or more) clueboxes
type Leader = (LineSegment 2 () Float, ClueBox) 


type instance IntersectionOf Leader Leader = [ NoIntersection, [Point 2 Float], [LineSegment 2 () Float]]

instance Leader `IsIntersectableWith` Leader where
    nonEmptyIntersection = defaultNonEmptyIntersection

    (ls,cb) `intersect` (ls_,cb_) =
      case second concat . first concat $ partitionEithers $ catMaybes [collect1 ls ls_,collect2 ls cb_, collect2 ls_ cb,collect3 cb cb_]  of
       ([],[])   -> coRec NoIntersection
       ([a],[]) -> coRec NoIntersection
       ([],[b]) -> coRec NoIntersection
       (a,_)    -> coRec a
       ([],b)   -> coRec b
       (a,b)     -> error $ "intersecting a line with a box. Box is degenerate" ++ show a ++ show b

      where
        collect1   :: LineSegment 2 () Float -> LineSegment 2 () Float -> Maybe (Either [Point 2 Float] [LineSegment 2 () Float])
        collect1 s1 s2 = match (s1 `intersect` s2) $
                        (H $ \NoIntersection           -> Nothing)
                     :& (H $ \(a :: Point 2 Float)         -> Just $ Left [a])
                     :& (H $ \(e :: LineSegment 2 () Float) -> Nothing)
                     :& RNil
        
        collect2   :: LineSegment 2 () Float -> ClueBox -> Maybe (Either [Point 2 Float] [LineSegment 2 () Float])
        collect2 s1 cb1 = match (cb1 `intersect` s1) $
                        (H $ \NoIntersection           -> Nothing)
                     :& (H $ \(a :: [Point 2 Float])         -> Just $ Left a)
                     :& (H $ \(e :: LineSegment 2 () Float) -> Nothing)
                     :& RNil

        collect3 :: ClueBox -> ClueBox -> Maybe (Either [Point 2 Float] [LineSegment 2 () Float])
        collect3 cb1 cb2 = match (cb1 `intersect` cb2) $
                        (H $ \NoIntersection           -> Nothing)
                     :& (H $ \(a :: [Point 2 Float])         -> Just $ Left a)
                     :& (H $ \(e :: [LineSegment 2 () Float]) -> Just $ Right e)
                     :& RNil


--------------------------------------------------------------------------

boxSize = 16
