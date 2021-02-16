{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE AllowAmbiguousTypes
-- #-}

module Algorithms.Geometry.Misc where

import Control.Lens

import Data.Vector ((!))
import Data.Foldable (toList)
import Data.List (sort)

import Data.Functor.Identity
import Data.Ext
import Data.Geometry hiding (head)
--import Data.Geometry.PlanarSubdivision
import qualified Data.CircularSeq as C
import qualified Data.List.NonEmpty as N

--import Data.PlanarGraph (PlanarGraph)
import Data.PlaneGraph (PlaneGraph, fromAdjRep, toAdjRep)
import Data.PlaneGraph.AdjRep hiding (id, vData, faces)
import qualified Data.PlaneGraph.AdjRep as A (id, vData, faces)

import Debug.Trace

import Data.Geometry.PlanarSubdivision.Dynamic
import Data.Geometry.PlanarSubdivision.More
import Data.Geometry.Polygon.Convex
import Data.Geometry.BezierSpline hiding (endPoints, reverse)

import Algorithms.Geometry.ConvexHull.GrahamScan

import           GHC.TypeNats


import           Data.LSeq (LSeq)
import qualified Data.LSeq as LSeq


average :: (Functor t, Foldable t, Arity d, Fractional r) => t (Point d r) -> Point d r
average ps = origin .+^ foldr1 (^+^) (fmap toVec ps) ^/ realToFrac (length ps)

class Central a where
  center :: Fractional r => a r -> Point 2 r

instance Central (Point 2) where
  center = id

instance Central (LineSegment 2 p) where
  center s = average $ map _core $ s ^. partsOf endPoints 

instance Central (SimplePolygon p) where
  center (SimplePolygon s) = average $ map _core $ toList s

{-
instance Central a => Central [a _] where
  center ls = average $ map center ls
-}



pythTrips :: [(Integer, Integer, Integer)]
pythTrips = [(3, 4, 5), (5, 12, 13), (11, 60, 61)]

rationalQuart :: Fractional r => [Vector 2 r]
rationalQuart = map (\(x, y, z) -> Vector2 (realToFrac x / realToFrac z) (realToFrac y / realToFrac z))
  $ (1, 0, 1) : map (\(x, y, z) -> (y, x, z)) (reverse pythTrips) ++ pythTrips

cardinals :: Num r => [Vector 2 r]
cardinals = [Vector2 1 0, Vector2 0 1, Vector2 (-1) 0, Vector2 0 (-1)]

rationalCircle :: Fractional r => [Vector 2 r]
rationalCircle = [commul c o | c <- cardinals, o <- rationalQuart]
 -- control density?

{-
rationalSemi :: Fractional r => [Vector 2 r]
rationalSemi = rationalQuart ++ [Vector2 (realToFrac 0) (realToFrac 1)] ++ map mirror1 rationalQuart

mirror1 :: Num r => Vector 2 r -> Vector 2 r
mirror1 (Vector2 x y) = Vector2 (-x) y
-- make more general?
-}

-- complex multiplication on vectors
commul :: Fractional r => Vector 2 r -> Vector 2 r -> Vector 2 r 
commul (Vector2 a b) (Vector2 c d) = Vector2 (a * c - b * d) (a * d + b * c)

dilate :: (Ord r, Fractional r) => LineSegment 2 v r -> r -> SimplePolygon () r
dilate s r = 
  let [p :+ _, q :+ _] = s ^. partsOf endPoints 
  in view simplePolygon $ convexHull $ N.fromList $ map (:+ ()) $ map ((p .+^) . (r *^)) rationalCircle ++ map ((q .+^). (r *^)) rationalCircle
  -- can do more efficiently using ConvexPolygon.merge

erode :: SomePolygon p r -> r -> SomePolygon p r
erode p _ = p


-- floating versions for actual equally spaced approximations

dilateFloating :: (Ord r, Floating r) => LineSegment 2 v r -> r -> SimplePolygon () r
dilateFloating s r = 
  let (p :+ _, q :+ _) = orderedEndPoints s 
      v = r *^ signorm (q .-. p)
      a = map (\t -> p .+^ rotate (t * pi / 180) v) $ map realToFrac [90, 120 .. 270]
      b = map (\t -> q .+^ rotate (t * pi / 180) v) $ map realToFrac [270, 300 .. 450]
  in SimplePolygon $ C.fromList $ map (\p -> p :+ ()) $ a ++ b

rotate :: Floating r => r -> Vector 2 r -> Vector 2 r 
rotate a (Vector2 x y) = Vector2 (cos a * x - sin a * y) (sin a * x + cos a * y)



-- moved to BezierSpline
{-



-- | Compute the convex hull of a 2-dimensional Bezier curve.
--   Should also work in any dimension, but convex hull is not yet implemented.
convexHullB :: (KnownNat n, Ord r, Fractional r) => BezierSpline (1 + n) 2 r -> ConvexPolygon () r
convexHullB b = convexHull $ N.fromList $ map (:+ ()) $ toList $ view controlPoints b

-- | Given two Bezier curves, list all intersection points.
--   Not exact, since for degree >= 3 there is no closed form.
--   (In principle, this algorithm works in any dimension
--   but this requires convexHull, area/volume, and intersect.)
intersectB :: (KnownNat n, Ord r, Fractional r) => BezierSpline (1 + n) 2 r -> BezierSpline (1 + n) 2 r -> [Point 2 r]
intersectB a b | a == b    = error "intersectB: equality" -- should return the curve
               | otherwise = let [a1, a2, a3, a4] = toList $ view controlPoints a
                                 [b1, b2, b3, b4] = toList $ view controlPoints b
                                 interior = subBezier treshold (1 - treshold)
                             in    intersectPointsPoints [a1, a4] [b1, b4]
                                ++ intersectPointsInterior [a1, a4] (interior b)
                                ++ intersectPointsInterior [b1, b4] (interior a)
                                ++ intersectInteriorInterior (interior a) (interior b)
  where treshold = 0.01                             

intersectPointsPoints :: Eq r => [Point 2 r] -> [Point 2 r] -> [Point 2 r]
intersectPointsPoints ps qs = filter (`elem` ps) qs

intersectPointsInterior :: (KnownNat n, Ord r, Fractional r) => [Point 2 r] -> BezierSpline (1 + n) 2 r -> [Point 2 r]
intersectPointsInterior ps b = filter (\p -> qdA p (snap b p) < treshold ^ 2) ps
  where treshold = 0.01
    
intersectInteriorInterior :: (KnownNat n, Ord r, Fractional r) => BezierSpline (1 + n) 2 r -> BezierSpline (1 + n) 2 r -> [Point 2 r]
intersectInteriorInterior a b = 
  let cha      = _simplePolygon $ convexHullB a
      chb      = _simplePolygon $ convexHullB b
      (a1, a2) = split 0.45 a
      (b1, b2) = split 0.55 b
      approx   = average [center cha, center chb]
      result | not (cha `intersectsP` chb) = []
             | area cha < treshold && area chb < treshold = [approx]
             | otherwise = intersectInteriorInterior a1 b1 
                        ++ intersectInteriorInterior a1 b2 
                        ++ intersectInteriorInterior a2 b1 
                        ++ intersectInteriorInterior a2 b2
                  in result 
  where treshold = 0.001

{-
type instance IntersectionOf (BezierSpline (1 + n) 2 r) (BezierSpline (1 + n) 2 r) = [ NoIntersection
                                                                                   , [Point 2 r]
                                                                                   , BezierSpline (1 + n) 2 r
                                                                                   ]


instance (KnownNat n, Ord r, Fractional r) => (BezierSpline (1 + n) 2 r) `IsIntersectableWith` (BezierSpline (1 + n) 2 r) where
  nonEmptyIntersection = defaultNonEmptyIntersection
  a `intersect` b = a `intersectB` b
-}

intersectsP :: (Ord r, Fractional r) => SimplePolygon p r -> SimplePolygon p r -> Bool
intersectsP p q = or [a `intersects` b | a <- p & listEdges, b <- q & listEdges]
               || (any (flip insidePolygon p) $ map _core $ toList $ polygonVertices q)
               || (any (flip insidePolygon q) $ map _core $ toList $ polygonVertices p)


splitMany :: forall n d r. (KnownNat n, Arity d, Ord r, Fractional r, Show r) => [r] -> BezierSpline (1 + n) d r -> [BezierSpline (1 + n) d r]
splitMany ts b | any (\x -> x < 0 || x > 1) ts = error $ "splitMany: invalid parameter value in " ++ show ts
               | otherwise = splitManySorted (sort $ filter (\x -> x > 0 && x < 1) ts) b
  where splitManySorted []       b = [b]
        splitManySorted (t : ts) b = (fst $ split t b) : splitMany (map (rescale t) ts) (snd $ split t b)
        rescale :: r -> r -> r
        rescale s t = t / (1 - s) + s

-}