-- Functionality to snap the vertices of a set of geometric objects to each other, so that vertices which are very close will be in fact at the same location.

module Algorithms.Geometry.Snap (snap, snapTo, snap', snapTo') where

import Algorithms.Geometry.Cluster

import Data.Geometry hiding (endPoints, head, init, _direction)
import Data.Geometry.Point
import Data.Geometry.BezierSpline hiding (snap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ext
import Data.Foldable (toList)
import Data.List

import Control.Monad
import Control.Lens

import GHC.TypeNats






-- | Takes a set of BezierSplines, and snaps their endpoints to each other if they are 
--	 closer than 'treshold' to another endpoint.
--   Would be nice if this worked on a generic set of geometric-objects-which-have-vertices...
snap :: (KnownNat n, Arity d, Ord r, RealFrac r, Show r) => r -> [BezierSpline n d r :+ e] -> [BezierSpline n d r :+ e]
snap treshold curves = map (core %~ snapTo points) curves
  where points = simplifyPoints treshold $ collectVertices $ map _core curves


-- | Given a set of curves, collect its vertices. That is, endpoints.
collectVertices :: (KnownNat n, Arity d, Ord r, Num r, Show r) => [BezierSpline n d r] -> [Point d r]
collectVertices = concatMap (\b -> let (p, q) = endPoints b in [p, q])

-- | Filter out points that are too close to each other.
simplifyPoints :: (Arity d, Ord r, Fractional r) => r -> [Point d r] -> [Point d r]
simplifyPoints treshold points = map last $ clusterCenter treshold $ removeDuplicates points

removeDuplicates :: Ord a => [a] -> [a] 
removeDuplicates = toList . Set.fromList





-- | Snap endpoints of a Bezier curve to a given set of points.
--   This function makes a linear pass over the points.
--   Should use the Voronoi diagram of the set of points to be efficient!
snapTo :: forall n d r. (KnownNat n, Arity d, Ord r, Num r, Show r) => [Point d r] -> BezierSpline n d r -> BezierSpline n d r
snapTo ps curve = let n = fromIntegral $ natVal (C @n)
                  in curve & controlPoints . ix 0 %~ closestPoint ps
                           & controlPoints . ix n %~ closestPoint ps

closestPoint :: (Arity d, Ord r, Num r, Show r) => [Point d r] -> Point d r -> Point d r
closestPoint points query = minimumBy (\p q -> compare (qdA query p) (qdA query q)) points






-- Removing Degeneracies

-- | Test whether a curve is *degenerate*, which could happen as a result of snapping.
--   For a Bezier curve, it tests whether the start and endpoint are the same.
isDegenerate :: (KnownNat n, Arity d, Ord r, Num r, Show r) => BezierSpline n d r -> Bool
isDegenerate b = let (p, q) = endPoints b in p == q



-- | Like 'snap', but removes any curves which after snapping become degenerate.
snap' :: (KnownNat n, Arity d, Ord r, RealFrac r, Show r) => r -> [BezierSpline n d r :+ e] -> [BezierSpline n d r :+ e]
snap' treshold curves = mfilter (not . isDegenerate . _core) $ snap treshold curves

-- | Like 'snapTo', but removes any curves which after snapping become degenerate.
snapTo' :: (KnownNat n, Arity d, Ord r, Num r, Show r) => [Point d r] -> BezierSpline n d r -> Maybe (BezierSpline n d r)
snapTo' ps curve = mfilter (not . isDegenerate) $ Just $ snapTo ps curve











-- Different approach to snapping: snap to a grid (without explicitly constructing it as a point set).

roundCurveAt :: RealFrac r => r -> BezierSpline 3 2 r :+ e -> BezierSpline 3 2 r :+ e
roundCurveAt r = core . controlPoints . traverse %~ roundPointAt r

roundPointAt :: RealFrac r => r -> Point 2 r -> Point 2 r
roundPointAt r = traverse %~ roundAt r

roundAt :: RealFrac r => r -> r -> r
roundAt r x = r * fromInteger (round $ x / r)
