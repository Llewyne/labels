module Convert where

import qualified Graphics.Gloss as G
import qualified Data.CircularSeq as C
import Data.Foldable
import Data.Geometry
import Data.Ext

import Data.Geometry.Point
import Data.Geometry.Polygon
import Data.Geometry.LineSegment
import Data.Geometry.Bezier


class Glossifiable a b where
  glossify :: a -> b

instance Real r => Glossifiable (Point 2 r) G.Point where
  glossify = glossifyPoint

instance Real r => Glossifiable (Polygon t p r) G.Picture where
  glossify = glossifyPolygon

instance Real r => Glossifiable (LineSegment 2 p r) G.Picture where
  glossify = glossifySegment

instance (Glossifiable a c, Glossifiable b c) => Glossifiable (Either a b) c where
  glossify (Left  l) = glossify l
  glossify (Right r) = glossify r


instance RealFrac r => Glossifiable (Bezier 2 r) G.Picture where
  glossify = glossifyBezier 10



glossifyPoint :: Real r => Point 2 r -> G.Point
glossifyPoint (Point2 x y) = (realToFrac x, realToFrac y)

glossifyPolygon :: Real r => Polygon t p r -> G.Picture
glossifyPolygon (SimplePolygon s) = G.Polygon $ glossifyCSeq s
glossifyPolygon (MultiPolygon s ps) = G.Pictures $ (G.Polygon $ glossifyCSeq s) : map (G.Color G.white . glossify) ps

glossifyCSeq :: Real r => C.CSeq (Point 2 r :+ p) -> [G.Point]
glossifyCSeq = map glossifyPoint . map _core . toList

glossifySegment :: Real r => LineSegment 2 p r -> G.Picture
glossifySegment s = let (p :+ _, q :+ _) = orderedEndPoints s 
                    in G.Line $ map glossifyPoint [p, q]

glossifyBezier :: RealFrac r => r -> Bezier 2 r -> G.Picture
glossifyBezier resolution b = G.Line $ map glossifyPoint $ approximate resolution b
