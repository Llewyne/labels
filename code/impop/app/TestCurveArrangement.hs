{-# LANGUAGE OverloadedStrings #-}

module TestCurveArrangement where

import Control.Lens
import Data.Ext

import Data.Default
import Data.Foldable (toList)
import Data.List

import Data.Geometry hiding (endPoints, head, init)
import Data.Geometry.Ipe
import Data.Geometry.Ipe.Types

import Data.Geometry.Point
import Data.Geometry.Polygon
import Data.Geometry.LineSegment hiding (endPoints)
import Data.Geometry.PlanarSubdivision hiding (endPoints)

import Data.Geometry.BezierSpline

import CurveArrangement
import CurveArrangement.Dynamic
import CurveArrangement.Loader




import Graphics.Gloss hiding (Point, Vector, Arc, Path, arc, display)
import Graphics.Gloss.Interface.Pure.Game hiding (Point, Vector, Arc, Path, arc)

import PSDGlossApp
import PSDGlossApp.Common
import PSDGlossApp.Visible
import PSDGlossApp.Forcible
import CurveArrangement
import CurveArrangement.Types
import CurveArrangement.Basic
import CurveArrangement.ForceDirectedStubs
import Data.Geometry.PlanarSubdivision.ForceDirected
import Data.Geometry.PlanarSubdivision.More

import Nonogram
import Nonogram.PathType

import Popular

import Convert

import PP.Puz

import Graphics.Gloss hiding (Point, Vector, Path, Arc, arc, display)

import Debug.Trace

main :: IO ()
main = do
  putStrLn "enter file name (without extension)"
  name <- getLine 
  car <- loadCAR ("ipe/" ++ name) :: IO (CA Rational)
  run ( id
      . forcibleStubs
      . forcible (curvedForces) -- simpleForces +++ 
--      . annotVisible 
--      . drawVertsWith basicDrawVert
      . overlayable
      . editable'
      . selectable 
      . hoverable . hoverableCurves
      . drawVertsWith forceDrawVert
      . drawCar
--      . basicVisible
      ) car


overlayable :: (Show r, RealFrac r) => CarBehaviour r
overlayable = handleEvent %~~ handleEdit
  where
    handleEdit (EventKey (Char 'm') Down _ _) s = s & subdivision %~ markPopularFaces
    handleEdit (EventKey (Char 'q') Down _ _) s = s & insertionState .~ (insertWholeCurveState (head $ overlay undefined) $ _subdivision s) 
    handleEdit (EventKey (Char 'w') Down _ _) s = s & insertionState %~ insertPiece
    handleEdit (EventKey (Char 'e') Down _ _) s = s & subdivision %~ insertWholeCurve (head $ overlay undefined)
    handleEdit (EventKey (Char 's') Down _ _) s = s & subdivision %~ simplify
    handleEdit _ s = s


-- | Remove any unnecessary degree-2 vertices.
simplify :: (Show r, RealFrac r) => CA r -> CA r
simplify ca = foldr simplifyVertex ca $ filter (degree 2) $ toList $ vertices' ca
  where
    degree k i = length (neighboursOf i ca) == k

simplifyVertex :: (Show r, RealFrac r) => VertexId' CAS -> CA r -> CA r
simplifyVertex j ca = 
  let [i, k] = toList $ neighboursOf i ca
      [ei]   = filter (\e -> tailOf e ca == i) $ common ca i j
      [ek]   = filter (\e -> tailOf e ca == j) $ common ca j k
      bi = edgeBezier ca ei
      bk = edgeBezier ca ek
  in case Just $ merge bi bk of -- note: right now merge might produce a non-similar curve! should be fixed here, or in "merge"?
       Nothing -> ca
       Just b  -> ca & mergeEdges ei ek (Just b)


{-
-- implementation assuming "merge" is implemented
simplifyVertex :: VertexId' CAS -> CA r -> CA r
simplifyVertex ca j = 
  let [i, k] = neighbours i ca
      ei = commonEdge i, j
      ek = commonEdge j, k
      bi = edgeBezier ei
      bk = edgeBezier ek
  in case merge bi bk of
       Nothing -> ca
       Just b  -> unSplitEdge ca i j k b
-}

---
--- Insert a whole new curve
---

type WholeCurve r = [BezierSpline 3 2 r]

data CurveInsertionState r = CurveInsertionState
  { car :: CA r
  , now :: VertexId' CAS
  , pie :: [BezierSpline 3 2 r]
  }

insertionState :: (Show r) => Lens' (State CAS CAV CAE CAF r) (CurveInsertionState r)
insertionState = lens getState setState

getState :: State CAS CAV CAE CAF r -> CurveInsertionState r
getState s = CurveInsertionState (_subdivision s) (case _finger s of V i -> i; _ -> toEnum 0) (_queue s)

setState :: (Show r) => State CAS CAV CAE CAF r -> CurveInsertionState r -> State CAS CAV CAE CAF r
setState s c = s & subdivision .~ car c
                 & finger      .~ (V $ now c)
                 & queue       .~ pie c




-- | Insert a whole curve into an existing curve arrangement.
insertWholeCurve :: (Ord r, RealFrac r, Show r) => WholeCurve r -> CA r -> CA r
insertWholeCurve [] ca = ca
insertWholeCurve wc ca = 
  let state = insertWholeCurveState wc ca
  in car $ while (not . null . pie) insertPiece state

insertWholeCurveState :: (Ord r, RealFrac r, Show r) => WholeCurve r -> CA r -> CurveInsertionState r 
insertWholeCurveState [] ca = error "insertWholeCurveState: empty curve"
insertWholeCurveState wc ca = 
  let startpoint   = fst $ endPoints $ head wc
      (car', now') = locatePoint ca startpoint
  in CurveInsertionState car' now' wc

-- | Locate a point in a curve arrangement: If the point is (close to) a vertex, return that
--   vertex id; if the point lies (almost) on an edge, split the edge and create a new vertex.
--   If the point is not on an edge, returns an error. (Should perhaps create new component?)
--   Currently takes linear time.
locatePoint :: (Ord r, RealFrac r, Show r) => CA r -> Point 2 r -> (CA r, VertexId' CAS)
locatePoint ca p = 
  let vs = filter (\i -> closeEnough p $ ca ^. locationOf i) $ toList $ vertices' ca
      es = filter (\i -> closeEnoughB p $ edgeBezier ca i) $ toList $ edges' ca
      result | not $ null vs = (ca, head vs)
             | not $ null es = locatePoint (subdivideEdge (head es) (parameterOf (edgeBezier ca $ head es) p) ca) p
             | otherwise     = error "locatePoint: interior point"
  in result



closeEnough :: (Arity d, Ord r, Fractional r, Show r) => Point d r -> Point d r -> Bool
closeEnough p q = qdA p q < treshold ^ 2
  where treshold = 0.01

closeEnoughB :: (Ord r, RealFrac r, Show r) => Point 2 r -> BezierSpline 3 2 r -> Bool
closeEnoughB p b = qdA p (snap b p) < treshold ^ 2
  where treshold = 0.01

degenerate :: (Ord r, Fractional r, Show r) => BezierSpline 3 2 r -> Bool
degenerate b = let (s, t) = endPoints b in closeEnough s t

-- | Insert one piece of a partial curve into an arrangement. Either creates a new vertex
--   in the current face and advances to the next piece, or creates a new vertex where
--   the current piece intersects an edge of the arrangement.
insertPiece :: (Ord r, RealFrac r, Show r) => CurveInsertionState r -> CurveInsertionState r
insertPiece state | null $ pie state              = error "insertPiece: empty pie"
                  | degenerate $ head $ pie state = state {pie = tail $ pie state}
                  | otherwise =
  let ca     = car state
      q      = ca ^. locationOf (now state)
      piece  = headTrace "insertPiece piece" $ pie state
      r      = fst $ endPoints piece -- q and r are equal, unless the arrangement is moving
      -- find the face that curve is pointing into
      f      = error "how do we find f?" -- head $ incidences ca $ now state
      curves = map (\i -> (i, edgeBezier ca i)) $ toList $ edges' ca -- incidences ca f
      points = concatMap (\(i, c) -> map (\p -> (i, p)) $ intersectB c piece) curves
      farpts = filter (\(i, p) -> not (closeEnough p q) && not (closeEnough p r)) points
      params = map (parameterOf piece) $ map snd farpts
      sppts  = sort $ zip params farpts
      (t, (i, p)) = headTrace "insertPiece (t, (i, p))" sppts
      result | null farpts = insertPieceInterior state f
             | otherwise   = insertPieceOnEdge state t i p 
  in result
  -- check whether piece intersects any boundary edge of the face
  -- if so, take the intersection point with minimum parameter value
  -- if not, insert into interior of current face

insertPieceInterior :: (Ord r, RealFrac r, Show r) => CurveInsertionState r -> FaceId' CAS -> CurveInsertionState r
insertPieceInterior state f =
  let b      = headTrace "insertPieceInterior b" $ pie state
      ca1    = car state
      i      = now state
      p      = snd $ endPoints b
      ca2    = dangleVertex i f p b ca1
      j      = snd $ locatePoint ca2 p -- can be more efficient
  in state { car = ca2
           , now = j
           , pie = tail $ pie state
           }

insertPieceOnEdge :: (Ord r, RealFrac r, Show r) => CurveInsertionState r -> r -> Dart CAS -> Point 2 r -> CurveInsertionState r
insertPieceOnEdge state t i p = 
  let (a, b) = split t (headTrace "insertPieceOnEdge (a, b)" $ pie state)
      ca1    = car state
      u      = parameterOf (edgeBezier ca1 i) p
      ca2    = subdivideEdge i u ca1
      j      = snd $ locatePoint ca2 p -- can be more efficient
      ca3    = insertEdge (now state) j (Just a) ca2
  in state { car = ca3
           , now = j
           , pie = b : (tail $ pie state)
           }


headTrace :: String -> [a] -> a
headTrace s xs | null xs   = error $ s ++ ": head of empty list"
               | otherwise = head xs


while :: (a -> Bool) -> (a -> a) -> a -> a
while test f x | test x    = while test f (f x)
               | otherwise = x



----------------------------------

overlay :: Fractional r => SimplePolygon v r -> [WholeCurve r]
overlay _ = [[Bezier3 (Point2 0 (249.23)) (Point2 0 (100)) (Point2 0 (-100)) (Point2 0 (-250.77))]]


----------------------------------




{-
-- restructure behaviours?
forcibleStubs :: CarBehaviour r
-}









{-
test :: String -> IO ()
test name = do
  page <- readIpePage ("ipe/" ++ name ++ ".ipe") :: IO (IpePage Rational)

  let segs = getSegments page
  writeFile "log/segments.txt" $ unlines $ map show segs

  let psd = fromConnectedSegments (Identity CAS) $ map (:+ ()) segs
  writeFile "log/psd.txt" $ show psd

  let neb = testneighbours psd
  putStrLn $ unlines $ map show neb

  putStrLn "happy"
-}


testneighbours :: PlanarSubdivision CAS v e f r -> [(FaceId' CAS, [FaceId' CAS])]
testneighbours ca = let ids = toList $ faces' ca
                    in zip ids $ map (neighbours ca) ids

neighbours :: PlanarSubdivision CAS v e f r -> FaceId' CAS -> [FaceId' CAS]
neighbours ca i = concatMap (\i -> [leftFace i ca]) $ toList $ outerBoundaryDarts i ca
                                   -- , rightFace i ca







readIpePage :: (Eq r, Coordinate r) => FilePath -> IO (IpePage r)
readIpePage filepath = do
  p <- readSinglePageFile filepath
  case p of
      Left err   -> error $ show err
      Right page -> return page

getSegments :: (Eq r, Fractional r, Show r) => IpePage r -> [LineSegment 2 () r]
getSegments = convertPathsToSegments . concatMap objectPaths . view content 

objectPaths :: IpeObject r -> [Path r :+ IpeAttributes Path r]
objectPaths (IpeGroup (Group os :+ _)) = concatMap objectPaths os
objectPaths (IpePath  (p        :+ a)) = [p :+ a]
objectPaths _                          = []

convertPathsToSegments :: Fractional r => [Path r :+ e] -> [LineSegment 2 () r]
convertPathsToSegments = concatMap (\(p :+ e) -> convertPathToSegments p)

convertPathToSegments :: Fractional r => Path r -> [LineSegment 2 () r]
convertPathToSegments = concatMap pathSegToLineSegs . toList . view pathSegments

pathSegToLineSegs :: Fractional r => PathSegment r -> [LineSegment 2 () r]
pathSegToLineSegs (PolyLineSegment (p)) = polyLineSegments p
pathSegToLineSegs _ = []

polyLineSegments :: PolyLine d p r -> [LineSegment d p r]
polyLineSegments pl = zipWith pointsToSeg (init $ toList $ pl ^. points) (tail $ toList $ pl ^. points)

pointsToSeg :: Point d r :+ p -> Point d r :+ p -> LineSegment d p r
pointsToSeg p q = LineSegment (Closed p) (Closed q)







testpoints :: [Point 2 Float]
testpoints = [ Point2 (-105.037    ) (46.0315  )
             , Point2 ( -53.7015   ) (98.152   )
             , Point2 (   4.7460003) (94.003   )
             , Point2 (  53.47     ) (78.177   )
             , Point2 (  20.0015   ) (39.0265  )
             , Point2 (  14.473001 ) (41.572   )
             , Point2 ( -45.008625 ) (55.105186)
             , Point2 (-105.037    ) (46.0315  )
             ]