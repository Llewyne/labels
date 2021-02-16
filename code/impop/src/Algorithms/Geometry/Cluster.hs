
module Algorithms.Geometry.Cluster ( clusterPairwise
                                   , clusterCenter
                                   , clusterPointsSpanningTree
                                   ) where

import Data.Geometry hiding (endPoints, head, init, _direction)
import Data.Geometry.Point
import Data.Tree
import qualified Data.List.NonEmpty as NonEmpty
import Data.Ext

import Algorithms.Geometry.EuclideanMST.EuclideanMST
import Algorithms.Geometry.Diameter.Naive

{-

import Debug.Trace
-}


-- | Input: a parameter 'delta' and a set of points.
--   Output: a set of clusters, with the following properties:
--   * every two points inside the same cluster are within distance 'delta';
--   * for every two distinct clusters there is a pair of points at distance larger than 'delta'.
--   Running time: O(n^2)
clusterPairwise :: (Arity d, Ord r, Fractional r) => r -> [Point d r] -> [[Point d r]]
clusterPairwise = clusterPointsSimple testAll

testAll :: (Arity d, Ord r, Fractional r) => r -> Point d r -> [Point d r] -> Bool
testAll delta p = all (\q -> qdA p q < delta ^ 2)

-- | Input: a parameter 'delta' and a set of points.
--   Output: a set of clusters, with the following properties:
--   * every cluster contains a *center* such that all points in a cluster are within distance 'delta' from its center;
--   * every two centers are at distance larger than 'delta'.
--   Running time: O(n^2)
clusterCenter :: (Arity d, Ord r, Fractional r) => r -> [Point d r] -> [[Point d r]]
clusterCenter = clusterPointsSimple testLast

testLast :: (Arity d, Ord r, Fractional r) => r -> Point d r -> [Point d r] -> Bool
testLast delta p qs = qdA p (last qs) < delta ^ 2





clusterPointsSimple :: (Arity d, Ord r, Fractional r) => (r -> Point d r -> [Point d r] -> Bool) -> r -> [Point d r] -> [[Point d r]]
clusterPointsSimple test delta [] = []
clusterPointsSimple test delta (p : ps) = insertPointSimple test delta p $ clusterPointsSimple test delta ps

insertPointSimple :: (Arity d, Ord r, Fractional r) => (r -> Point d r -> [Point d r] -> Bool) -> r -> Point d r -> [[Point d r]] -> [[Point d r]]
insertPointSimple test delta p [] = [[p]]
insertPointSimple test delta p (c : cs) | test delta p c = (p : c) : cs
                                        | otherwise      = c : insertPointSimple test delta p cs


-- | Cluster a set of points using parameters c and delta such that
--   * within each cluster, all points are delta-connected, and
--   * all points in a cluster have diameter at most c times delta.
-- More efficient implementation using MSTs.
clusterPointsSpanningTree :: (Ord r, Fractional r) => r -> r -> [Point 2 r] -> [[Point 2 r]]
clusterPointsSpanningTree delta c points = clusterTree delta c $ makeTree points

makeTree :: (Ord r, Fractional r) => [Point 2 r] -> Tree (Point 2 r)
makeTree points = fmap _core $ euclideanMST $ NonEmpty.fromList $ map (:+ ()) $ points

-- | Return a clustering of the points in the tree (see clusterPoints).
-- The root is always contained in the first element of the list.
clusterTree :: (Ord r, Fractional r) => r -> r -> Tree (Point 2 r) -> [[Point 2 r]]
clusterTree delta c (Node p ts) = 
  let css = map (clusterTree delta c) ts
      rot = insertPoint delta c p $ map head css
      res = concatMap tail css
  in rot ++ res

insertPoint :: (Ord r, Fractional r) => r -> r -> Point 2 r -> [[Point 2 r]] -> [[Point 2 r]]
insertPoint delta c p clusters = 
  let far = filter (all (\q -> qdA p q >  delta ^ 2)) clusters
      clo = filter (any (\q -> qdA p q <= delta ^ 2)) clusters
  in foldr (mergeClusters delta c) [[p]] clo ++ far

mergeClusters :: (Ord r, Fractional r) => r -> r -> [Point 2 r] -> [[Point 2 r]] -> [[Point 2 r]]
mergeClusters delta c new (old : rest) | smaller (new ++ old) (delta * c) = (new ++ old) : rest
                                       | otherwise                        = old : new : rest

smaller :: (Ord r, Num r) => [Point 2 r] -> r -> Bool
smaller ps r = case diametralPair $ map (:+ ()) ps 
               of Nothing -> True 
                  Just (p :+ (), q :+ ()) -> qdA p q < r ^ 2

