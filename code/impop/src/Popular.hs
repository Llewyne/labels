module Popular where


import Control.Lens
import Data.Ext
import Data.Foldable (toList)

import Data.Geometry.Polygon
import Data.Geometry.PlanarSubdivision hiding (endPoints)
import Data.Geometry.PlanarSubdivision.More
import Data.Geometry.BezierSpline

import Graphics.Gloss hiding (Point, Vector, Arc, Path, arc, display)

import PSDGlossApp
import PSDGlossApp.Visible

import CurveArrangement.Types
import CurveArrangement.Basic

import Nonogram
import Nonogram.PathType

import Convert

import PP.Puz

import Debug.Trace

---------------------------
-- finding popular faces --
---------------------------

markPopularFaces :: CA r -> CA r
markPopularFaces ca = foldr markPopularFacesFrom (erasePopularFaces ca) $ entryDarts ca

markPopularFacesFrom :: Dart CAS -> CA r -> CA r
markPopularFacesFrom d ca = let curve = traceCurve ca d
                                faces = map fst $ filter snd $ markDuplicates $ removeConsecutiveDuplicates $ leftFaces ca curve
                            in foldr (\i c -> c & dataOf i . popular .~ True) ca faces

erasePopularFaces :: CA r -> CA r
erasePopularFaces ca = foldr (\i c -> c & dataOf i . popular .~ False) ca $ faces' ca

markDuplicates :: Eq a => [a] -> [(a, Bool)]
markDuplicates xs = map (\x -> (x, count x xs >= 2)) xs

count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (== x) xs

----------------------------------------------------
-- drawing arrangements with popular faces marked --
----------------------------------------------------

type CarBehaviour r = Behaviour CAS CAV CAE CAF r


drawCar :: (RealFrac r, Show r) => CarBehaviour r
drawCar = id
        . drawEdgesWith drawCarEdge -- but also base color on edge info
        . drawFacesWith drawCarFace-- is it a solution face, and is it impopular

drawCarEdge :: (RealFrac r, Show r) => EdgeDrawer CAS CAV CAE CAF r
drawCarEdge _ _ (e1, e2) s = let fp = e1 ^. nonoe . framePath
                                 bp = e2 ^. nonoe . boundaryPath
--                                 c | fp = blue
--                                   | bp = red
--                                   | otherwise = black
                                 c = black
                                 w | fp = 3
                                   | bp = 2
                                   | otherwise = 1
                                 r = 1 
                             in Color c $ glossifyBezier r w $ edgeDataBezier (e1, e2) s

{-
type VertDrawer s v r   = RealFrac r => VertexId' s -> v      -> Point 2 r         -> Picture
type EdgeDrawer s v e r = RealFrac r => Arc s       -> (e, e) -> LineSegment 2 v r -> Picture
type FaceDrawer s v f r = RealFrac r => FaceId' s   -> f      -> SomePolygon v r   -> Picture
-}

drawCarFace :: (RealFrac r, Show r) => FaceDrawer CAS CAV CAE CAF r
drawCarFace ca i f p | not (f ^. popular) = Blank
                     | otherwise =
                       let pol = curvedFacePolygon ca i
                           api = glossify pol
                           c = id
--                             $ mix (f ^. full) blue
                             $ mix (f ^. popular) red
                             $ white
                       in Color c api

mix :: Bool -> Color -> Color -> Color
mix True new old = mixColors 0.8 0.2 old new
mix False _ old = old

tr :: Show a => String -> a -> a
tr s a = trace ("\9608 " ++ s ++ ": " ++ show a) a

curvedFacePolygon :: (RealFrac r, Show r) => CA r -> FaceId' CAS -> SimplePolygon () r 
curvedFacePolygon ca i = 
  let curves = map (edgeBezier ca) $ toList $ outerBoundaryDarts i ca
      points = tail $ removeConsecutiveDuplicates $ concat $ map (approximate resolution) curves
  in fromPoints $ map (:+ ()) points
  where resolution = 5

-- fromPoints :: [Point 2 r :+ p] -> SimplePolygon p r
