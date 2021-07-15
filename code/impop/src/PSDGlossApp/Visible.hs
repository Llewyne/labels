
module PSDGlossApp.Visible where

import Control.Lens

import Data.Ext
import Data.Foldable (toList)

import Graphics.Gloss hiding (Point, Vector, Arc, arc, display)

import Data.Geometry hiding (edgeSegments)
import Data.Geometry.PlanarSubdivision
import Data.PlanarGraph.Dart

import Algorithms.Geometry.Misc

import Glossify
import Draw

import PSDGlossApp.Common

drawOver :: Picture -> Picture -> Picture
drawOver a b = Pictures [b, a]

drawUnder :: Picture -> Picture -> Picture
drawUnder a b = Pictures [a, b]

type VertDrawer s v e f r = RealFrac r => PlanarSubdivision s v e f r -> VertexId' s -> v      -> Point 2 r         -> Picture
type EdgeDrawer s v e f r = RealFrac r => PlanarSubdivision s v e f r -> Arc s       -> (e, e) -> LineSegment 2 v r -> Picture
type FaceDrawer s v e f r = RealFrac r => PlanarSubdivision s v e f r -> FaceId' s   -> f      -> SomePolygon v r   -> Picture

drawVertsWith :: RealFrac r => VertDrawer s v e f r -> Behaviour s v e f r
drawEdgesWith :: RealFrac r => EdgeDrawer s v e f r -> Behaviour s v e f r
drawFacesWith :: RealFrac r => FaceDrawer s v e f r -> Behaviour s v e f r

drawVertsWith f = drawState %~ (\g st -> Pictures $ g st : vps st)
  where vps st = map (\(i, VertexData p v) -> f (_subdivision st) i v p) $ toList $ vertices $ _subdivision st

drawEdgesWith f = drawState %~ (\g st -> Pictures $ g st : eps st)
  where eps st = map (\(i, s :+ e) -> f (_subdivision st) (i ^. arc) (e, _subdivision st ^. dataOf (twin i)) s) $ toList $ edgeSegments $ _subdivision st
--  where eps st = map (\i -> f (i ^. arc) (_subdivision st ^. dataOf i, _subdivision st ^. dataOf (twin i)) (_core $ _subdivision st & edgeSegment i)) $ toList $ edges' $ _subdivision st

drawFacesWith f = drawState %~ (\g st -> Pictures $ g st : fps st)
  where fps st = map (\(i, p :+ e) -> f (_subdivision st) i e p) $ toList $ rawFacePolygons $ _subdivision st

basicDrawVert :: VertDrawer s v e f r
basicDrawVert _ _ _ p = Color black $ uncurry Translate (glossify p) $ circleSolid 3

basicDrawEdge :: EdgeDrawer s v e f r
basicDrawEdge _ _ _ s = Color edgeColor $ glossify s

edgeColor = makeColor 0.7 0.7 0.8 1.0

basicDrawFace :: FaceDrawer s v e f r
basicDrawFace _ _ _ _ = Blank

-- add basic drawing functionality
-- visible :: (RealFrac r, HasPSD s v e f r a) => GlossApp a -> GlossApp a
basicVisible :: RealFrac r => Behaviour s v e f r
basicVisible = drawVertsWith basicDrawVert . drawEdgesWith basicDrawEdge . drawFacesWith basicDrawFace
-- ^ split into basic version and version where you specify how to draw?




drawAnnotation :: (Central c, RealFrac r, Enum i) => Color -> i -> e -> c r -> Picture
drawAnnotation c i _ p = uncurry Translate (glossify $ center p) $ Pictures
                       [ Color c $ circleSolid $ 100 * fontsize
                       , Color white $ draw $ show $ fromEnum i
                       , Color black $ circle $ 100 * fontsize
                       ] 

annotDrawVert :: VertDrawer s v e f r
annotDrawVert _ = drawAnnotation blue

annotDrawEdge :: EdgeDrawer s v e f r
annotDrawEdge _ = drawAnnotation red

annotDrawFace :: FaceDrawer s v e f r
annotDrawFace _ i e f = drawAnnotation green i e $ either id (view outerBoundary) f

annotVisible :: RealFrac r => Behaviour s v e f r
annotVisible = drawVertsWith annotDrawVert . drawEdgesWith annotDrawEdge . drawFacesWith annotDrawFace

