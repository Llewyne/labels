module Data.Geometry.PlanarSubdivision.More where

import           Data.PlaneGraph (PlaneGraph)
import qualified Data.PlaneGraph as PG
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.PlanarSubdivision.Raw

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Foldable as F
import           Data.Vector (Vector, toList, (//), empty)
import qualified Data.Vector as V
import           Data.Set (Set)
import qualified Data.Set as Set


-- opposite darts

-- | For a given curve arrangement, list all darts pointing into the interior (start of curves).
--   Currently inspects all vertices. Should be made more efficient by only checking boundary
--   vertices, but this fails if the arrangement gets updated (for some reason).
entryDarts :: PlanarSubdivision s v e f r -> [Dart s]
entryDarts psd = let vs = vertices' psd -- toList $ boundaryVertices (outerFaceId psd) psd
                     es = concatMap (\v -> filter (\e -> tailOf e psd == v) $ incidences psd v) vs
                 in filter (\e -> leftFace e psd /= outerFaceId psd && rightFace e psd /= outerFaceId psd) es


-- | Given a dart d, return the complete path of 'oppositeIncidentEdge' adjacencies. 
--   Could be a cycle, or a path that starts and ends at odd degree vertices.
traceCurve :: PlanarSubdivision s v e f r -> Dart s -> [Dart s]
traceCurve psd d | init b == tail f = f
                 | otherwise        = b ++ tail f
  where b = traceCurveBackward psd d 
        f = traceCurveForward  psd d

traceCurveBackward :: PlanarSubdivision s v e f r -> Dart s -> [Dart s]
traceCurveBackward psd d = reverse $ map twin $ traceCurveForward psd $ twin d

traceCurveForward :: PlanarSubdivision s v e f r -> Dart s -> [Dart s]
traceCurveForward psd d = uncycle $ repeatedly (oppositeIncidentEdge psd) d

-- | Take elements from a list until the first time the first element appears again.
uncycle :: Eq a => [a] -> [a]
uncycle [] = []
uncycle (x : xs) = x : f x xs
  where f x [] = []
        f x (y : ys) | x == y = []
                     | otherwise = y : f x ys

-- | Keep applying the function to transform the value, until it yields
--   Nothing.  Returns the sequence of transformed values.
repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f x = x : case f x of Nothing -> []
                                 Just y  -> repeatedly f y

-- | Given a dart d that points into some vertex v of even degree, report the opposite dart e in the cyclic order around v, coming out of v.
oppositeIncidentEdge :: PlanarSubdivision s v e f r -> Dart s -> Maybe (Dart s)
oppositeIncidentEdge psd d | even n = Just $ head $ filter (\e -> tailOf e psd == v) $ commonDarts psd v w
  where u = tailOf d psd
        v = headOf d psd
        n = degree psd v
        w = nebs !! (n `div` 2)
        nebs = dropWhile (/= tailOf d psd) $ cycle $ toList $ neighboursOf (headOf d psd) psd
oppositeIncidentEdge psd d | otherwise = Nothing

{-
oppositeIncidentEdge :: PlanarSubdivision s v e f r -> Dart s -> Maybe (Dart s)
oppositeIncidentEdge psd d | even n = Just $ twin $ iterate (flip nextIncidentEdge psd) (d) !! (n `div` 2)
  where n = degree psd $ headOf d psd
oppositeIncidentEdge psd d | otherwise = Nothing
-}

-- | Get the degree of a vertex.
degree :: PlanarSubdivision s v e f r -> VertexId' s -> Int
degree psd i = length $ neighboursOf i psd





applyAllV :: (VertexId' s -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r) -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
applyAllV f psd = foldr ($) psd $ map f $ toList $ vertices' psd

applyAllD :: (Dart s -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r) -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
applyAllD f psd = foldr ($) psd $ map f $ toList $ darts' psd

applyAllF :: (FaceId' s -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r) -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
applyAllF f psd = foldr ($) psd $ map f $ toList $ faces' psd

class Applicable s a where
  applyAll :: (a -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r) -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r

instance Applicable s (VertexId' s) where applyAll = applyAllV
instance Applicable s (Dart s)      where applyAll = applyAllD
instance Applicable s (FaceId' s)   where applyAll = applyAllF






