module CurveArrangement.Construct where


import Control.Lens
import Data.List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Tree
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default
import Data.Foldable (toList)
import Data.Ext

import Data.Geometry.Point
import Data.Geometry.Vector hiding (head, init, last)
import Data.Geometry.BezierSpline hiding (snap)
import Data.Geometry.LineSegment hiding (endPoints)
import Data.Geometry.PlanarSubdivision hiding (endPoints)
import Data.PlanarGraph.Dart 

import Data.Geometry.PlanarSubdivision.ForceDirected
import Data.Geometry.PlanarSubdivision.More

import Data.Geometry hiding (endPoints, head, init, _direction)
import qualified Data.CircularSeq as C

import Convert

import Algorithms.Geometry.EuclideanMST.EuclideanMST
import Algorithms.Geometry.Diameter.Naive
import Algorithms.Geometry.Misc

import CurveArrangement.Types
--import CurveArrangement.ForceDirectedStubs

import Nonogram.PathType


import Misc.SpanningTree
import Data.Tree


import Debug.Trace

tracingOn = True

tr :: Show a => String -> a -> a
tr s a | tracingOn = trace ("\9608 " ++ s ++ ": " ++ show a) a
       | otherwise = a

-- | Take a bag of curves, and build their arrangement.
construct :: (Ord r, RealFrac r, Show r) => [BezierSpline 3 2 r :+ NonoPathType] -> CA r
--construct cs = constructPlanar $ tr "construct: " $ snap $ chop cs
construct = constructPlanar . chopQuadratic

-- incremental might be the way to go
-- given a CAr of curves, and a single curve to insert...
-- requires : dealing with disconnected cars
--          : efficient insertion in car        


-- sweep line approach
----------------------

-- | Take a bag of possibly intersecting curves, and subdivide them so they 
--   are no longer intersecting. Using a sweep-line approach.
chopSweepLine :: (Ord r, RealFloat r, Show r) => [BezierSpline 3 2 r :+ e] -> [BezierSpline 3 2 r :+ e]
chopSweepLine curves = chopSweepLineMonotone $ concatMap (\(c :+ e) -> map (:+ e) $ splitMonotone 2 c) curves

chopSweepLineMonotone :: (Ord r, RealFrac r, Show r) => [BezierSpline 3 2 r :+ e] -> [BezierSpline 3 2 r :+ e]
chopSweepLineMonotone = undefined -- use Algorithms.Geometry.CurveArrangement.BentleyOttmann

{-
map monotone 1 curves -> now all curves are x-monotone
sweep from left to right
-}


-- O(n^2 + k) approach
----------------------

chopQuadratic :: (Ord r, RealFrac r, Show r) => [BezierSpline 3 2 r :+ e] -> [BezierSpline 3 2 r :+ e]
chopQuadratic curves = concat $ zipWith subdivide curves $ intersectionPoints $ map _core curves

-- first find all intersection points -> need to remember which points belong to which curves
intersectionPoints :: (Ord r, RealFrac r, Show r) => [BezierSpline 3 2 r] -> [[Point 2 r]]
intersectionPoints curves = 
  let n        = length curves
      pairs    = [(i, j) | j <- [0 .. n - 1], i <- [0 .. j - 1]]
      pts i j  = intersectB (curves !! i) (curves !! j)
      f (i, j) = zip (repeat i) (pts i j) ++ zip (repeat j) (pts i j)
      points   = concatMap f pairs
  in map removeDuplicates $ groupByFst $ sort points
  -- how can there be duplicates ?!?
  -- same point can be reported by multiple other curves
  -- need to handle case where they are not exactly equal but very close...

groupByFst :: Eq a => [(a, b)] -> [[b]]
groupByFst = (map . map) snd . groupBy (\x y -> fst x == fst y)


{-
collect :: Eq a => [(a, [b])] -> [[b]]
collect things = map goget'm $ groupBy (\x y -> fst x == fst y) things 

goget'm :: [(a, [b])] -> [b]
goget'm = concat . map snd
-}

-- then subdivide curves based on those points
subdivide :: (Ord r, RealFrac r, Show r) => BezierSpline 3 2 r :+ e -> [Point 2 r] -> [BezierSpline 3 2 r :+ e]
subdivide (curve :+ e) points = map (:+ e) $ splitByPoints points curve

-- O(n^3) approach
------------------


-- subdivide curves if crossing
chop :: (Ord r, RealFrac r, Show r, Eq e) => [BezierSpline 3 2 r :+ e] -> [BezierSpline 3 2 r :+ e]
chop curves = concatMap (\c -> manyChopOne (filter (/= c) curves) c) curves

-- instead of first chopping and then snapping, more robust to do it all at once?
-- while there are still intersecting pairs of curves, intersect and immediately snap?
-- also: needs to be faster
-- how? sweepline?

manyChopOne :: (Ord r, RealFrac r, Show r, Eq e) => [BezierSpline 3 2 r :+ e] -> BezierSpline 3 2 r :+ e -> [BezierSpline 3 2 r :+ e]
manyChopOne slayers victim = manyChopMany slayers [victim]
manyChopMany :: (Ord r, RealFrac r, Show r, Eq e) => [BezierSpline 3 2 r :+ e] -> [BezierSpline 3 2 r :+ e] -> [BezierSpline 3 2 r :+ e]
manyChopMany slayers victims = foldr ($) victims $ map oneChopMany slayers
oneChopMany :: (Ord r, RealFrac r, Show r, Eq e) => BezierSpline 3 2 r :+ e -> [BezierSpline 3 2 r :+ e] -> [BezierSpline 3 2 r :+ e]
oneChopMany slayer victims = concatMap (oneChopOne slayer) victims
oneChopOne :: (Ord r, RealFrac r, Show r, Eq e) => BezierSpline 3 2 r :+ e -> BezierSpline 3 2 r :+ e -> [BezierSpline 3 2 r :+ e]
oneChopOne (slayer :+ _) (victim :+ b) = map (roundCurveAt treshold) $ map (:+ b) $ splitMany (map (parameterOf victim) $ intersectB slayer victim) victim
  where
    treshold = 0.001

-- snap sufficiently close endpoints to the same point
snap :: (Ord r, RealFrac r, Show r) => [BezierSpline 3 2 r :+ e] -> [BezierSpline 3 2 r :+ e]
snap curves = filter (not . isLoop) $ map (snapToPoints $ simplifyPoints $ collectEndpoints curves) curves

isLoop :: (Ord r, Num r, Show r) => BezierSpline 3 2 r :+ e -> Bool
isLoop (b :+ e) = let (p, q) = endPoints b in p == q

collectEndpoints :: [BezierSpline 3 2 r :+ e] -> [Point 2 r]
collectEndpoints = concatMap (\b -> extremes $ toList $ view controlPoints $ _core b)


roundCurveAt :: RealFrac r => r -> BezierSpline 3 2 r :+ e -> BezierSpline 3 2 r :+ e
roundCurveAt r = core . controlPoints . traverse %~ roundPointAt r

roundPointAt :: RealFrac r => r -> Point 2 r -> Point 2 r
roundPointAt r = traverse %~ roundAt r

roundAt :: RealFrac r => r -> r -> r
roundAt r x = r * fromInteger (round $ x / r)

extremes :: [a] -> [a]
extremes [] = []
extremes xs = head xs : last xs : []

-- | Filter out points that are too close.
simplifyPoints :: (Ord r, Fractional r) => [Point 2 r] -> [Point 2 r]
simplifyPoints points = map average $ clusterPoints treshold 5 $ removeDuplicates points
  where 
    treshold = 0.5

removeDuplicates :: Ord a => [a] -> [a] 
removeDuplicates = toList . Set.fromList

-- | Cluster a set of points using parameters c and delta such that
--   * within each cluster, all points are delta-connected, and
--   * all points in a cluster have diameter at most c times delta.
clusterPoints :: (Ord r, Fractional r) => r -> r -> [Point 2 r] -> [[Point 2 r]]
clusterPoints delta c points = clusterTree delta c $ makeTree points

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

-- | Snap endpoints of a Bezier curve to a given set of points.
--   This function makes a linear pass over the points.
--   Should use the Voronoi diagram of the set of points to be efficient!
snapToPoints :: (Ord r, Num r) => [Point 2 r] -> BezierSpline 3 2 r :+ e -> BezierSpline 3 2 r :+ e
snapToPoints ps (curve :+ b) = let [p1, p2, p3, p4] = toList $ view controlPoints curve
                               in Bezier3 (closestPoint ps p1) p2 p3 (closestPoint ps p4) :+ b

closestPoint :: (Ord r, Num r) => [Point 2 r] -> Point 2 r -> Point 2 r
closestPoint points query = minimumBy (\p q -> compare (qdA query p) (qdA query q)) points











-- | Take a set of curves; if any pair shares the same endpoints, split one of them.
avoidMultiEdges :: (Ord r, Fractional r, Show r) => [BezierSpline 3 2 r :+ e] -> [BezierSpline 3 2 r :+ e]
avoidMultiEdges cs = concatMap (\(c :+ e) -> map (:+ e) $ splitIfMultiEdge (map _core cs) c) cs

splitIfMultiEdge :: (Ord r, Fractional r, Show r) => [BezierSpline 3 2 r] -> BezierSpline 3 2 r -> [BezierSpline 3 2 r]
splitIfMultiEdge cs c | any (\d -> sharesEndpoints c d && c /= d) cs = splitMany [0.5] c 
                      | isLoop (c :+ ())                             = splitMany [0.5] c 
                      | otherwise                                    = [c]

sharesEndpoints :: (Ord r, Fractional r, Show r) => BezierSpline 3 2 r -> BezierSpline 3 2 r -> Bool
sharesEndpoints a b = let (a1, a2) = endPoints a
                          (b1, b2) = endPoints b 
                      in (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1)

-- | Take a connected set of non-intersecting curves, and build their arrangement.
constructPlanar :: (Ord r, RealFrac r, Show r) => [BezierSpline 3 2 r :+ NonoPathType] -> CA r
constructPlanar cs = glue $ map seg $ avoidMultiEdges cs




-- converting to segments for use in from ConnectedSegments

seg :: Real r => BezierSpline 3 2 r :+ NonoPathType -> LineSegment 2 CAV r :+ (CAE, CAE)
seg (b :+ f) = let [p1, p2, p3, p4] = b ^. controlPoints . to toList
                   [f1, f2, f3, f4] = map (traverse %~ realToFrac) [p1, p2, p3, p4]
                   v1 | isFixed f = CAV (Fixed f1) (origin .-. origin)
                      | otherwise = def
                   v4 | isFixed f = CAV (Fixed f4) (origin .-. origin)
                      | otherwise = def
                   e1 = CAE (isFixed f) (f2 .-. f1) f
                   e4 = CAE (isFixed f) (f3 .-. f4) f
               in LineSegment (Closed (p1 :+ v1)) (Closed (p4 :+ v4)) :+ (e1, e4)


glue :: (Ord r, Fractional r) => [LineSegment 2 CAV r :+ (CAE, CAE)] -> CA r
glue ls = fromConnectedSegments (Identity CAS) ls
        & vertexData . traverse %~ reconcile . toList 
        & dartData   . traverse %~ selectHalfEdge
        & faceData   . traverse .~ def

-- | Combine vertex data: fixed takes preference over constrained over fluid.
reconcile :: [CAV] -> CAV
reconcile = flip CAV (origin .-. origin) . foldr1 recombine . map _fl
  where recombine :: Fluidity -> Fluidity -> Fluidity
        recombine a@(Fixed _)         _                   = a
        recombine _                   b@(Fixed _)         = b
        recombine a@(Constrained _ _) _                   = a
        recombine _                   b@(Constrained _ _) = b
        recombine _                   _                   = Fluid

-- | Select the correct edge data, depending on directino of dart.
selectHalfEdge :: (Dart CAS, (CAE, CAE)) -> (Dart CAS, CAE)
selectHalfEdge (d, (e1, e2)) | _direction d == Positive = (d, e1)
                             | otherwise                = (d, e2)

{-
Fluid | Fixed (Point 2 Float)

data CAV = CAV
  { _fl :: Fluidity
  , _fv :: ForceVector
  } deriving (Show, Eq)
$(makeLenses ''CAV)

data CAE = CAE
  { _froz :: Bool
  , _stub :: Vector 2 Float -- would prefer :: Vector 2 r ?
  } deriving (Show, Eq)
$(makeLenses ''CAE)
-}







-- | Find out which faces are filled by performing a DFS and checking which edge are part of boundary.
fillFaces :: CA r -> CA r
fillFaces ca = 
  let root = outerFaceId ca 
      tree = spanningTree (neighbours ca) root
      ids  = traverseTree ca tree False
  in foldr (\i -> dataOf i . full .~ True) ca ids


faceTree :: CA r -> Tree (FaceId' CAS)
faceTree ca = spanningTree (neighbours ca) $ outerFaceId ca


traverseTree :: CA r -> Tree (FaceId' CAS) -> Bool -> [FaceId' CAS]
traverseTree ca (Node i children) f = (if f then [i] else []) ++ concatMap recurse children
  where
    recurse :: Tree (FaceId' CAS) -> [FaceId' CAS]
    recurse t@(Node j _) | _boundaryPath $ _nonoe $ ca ^. dataOf (head $ commonDarts ca i j)
                                     = traverseTree ca t (not f)
                         | otherwise = traverseTree ca t f


testneighbours :: CA r -> [(FaceId' CAS, [FaceId' CAS])]
testneighbours ca = let ids = toList $ faces' ca
                    in zip ids $ map (neighbours ca) ids

neighbours :: CA r -> FaceId' CAS -> [FaceId' CAS]
neighbours ca i = concatMap (\i -> [leftFace i ca]) $ toList $ outerBoundaryDarts i ca
                                   -- , rightFace i ca




-- where does this belong? This is not about construction...
fullFaces :: CA r -> [FaceId' CAS]
fullFaces ca = filter (\i -> ca ^. dataOf i . full) $ toList $ faces' ca
