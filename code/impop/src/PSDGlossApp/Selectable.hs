
module PSDGlossApp.Selectable where

import Control.Lens

import Graphics.Gloss hiding (Point, Vector, Arc, arc, display)
import Graphics.Gloss.Interface.Pure.Game hiding (Point, Vector, Arc, arc)

import Data.Ext
import Data.Foldable (toList)
import Data.Geometry hiding (head, edgeSegments)
import Data.Geometry.PlanarSubdivision
import Data.Geometry.PlanarSubdivision.More
import Data.PlanarGraph.Dart

import Algorithms.Geometry.Misc

import Glossify
import Draw

import PSDGlossApp.Common
import PSDGlossApp.Visible


-- hoverable = 


hoverDrawVert :: VertDrawer s v e f r
hoverDrawVert _ i v p = Color yellow $ uncurry Translate (glossify p) $ circleSolid 15

hoverDrawEdge :: EdgeDrawer s v e f r
hoverDrawEdge _ i e s = Color yellow $ glossify $ dilate s 5

hoverDrawFace :: FaceDrawer s v e f r
hoverDrawFace _ i f p = Color yellow $ glossify $ erode p 5

--drawHover :: RealFrac r => PlanarSubdivision s v e f r -> PSE s -> Picture
--drawHover psd N     = Blank
--drawHover psd (V i) = drawAt (psd ^. locationOf i) hovV
--drawHover psd (D i) = hovE (psd ^. locationOf (psd & headOf i)) (psd ^. locationOf (psd & tailOf i))


hoverable :: RealFrac r => Behaviour s v e f r
hoverable = (drawState %~ (\f st -> drawOver (f st) (drawHover (_subdivision st) (_hover st))))
          . (handleEvent %~~ handleHover)
--  { _drawState   = \s -> Pictures [drawHover (subdivision s) (hover s), drawState app s]
--  , _handleEvent = \e -> handleHover e . handleEvent app e
--  , passTime    = passTime app -- check for update?
--  }
  where
--    drawHover :: RealFrac r => PlanarSubdivision s v e f r -> PSE s -> Picture
    drawHover psd N     = Blank
    drawHover psd (V i) = hoverDrawVert psd i (psd ^. dataOf i) (psd ^. locationOf i)
    drawHover psd (D i) = hoverDrawEdge psd (_arc i) (psd ^. dataOf i, psd ^. dataOf (twin i)) $ _core $ edgeSegment i psd
    drawHover psd (F i) = hoverDrawFace psd i (psd ^. dataOf i) $ _core $ rawFacePolygon i psd


--    handleHover :: Event -> State s v e f r -> State s v e f r
    handleHover (EventMotion (x, y)) s = 
      let h = _hover s
          n = locate (_subdivision s) $ traverse %~ realToFrac $ Point2 x y
      in  s { _hover = n, _hoverChanged = h /= n }
    handleHover _ s = s

-- should we make explicit depth order? we like hover below selection below normal drawing, but selection behaviour is added last!
-- or, visualize selection differently?

hoverableCurves :: RealFrac r => Behaviour s v e f r
--hoverable :: RealFrac r => App s v e f r -> App s v e f r
-- ^ should be more general
hoverableCurves = drawState %~ \f s -> drawOver (f s) $ Color red $ drawCurve (_subdivision s) (_hover s)
  where
--    curveVerts psd i = flip tailOf psd (head $ traceCurve psd i) : (map (flip headOf psd) $ traceCurve psd i)
    drawCurve psd (D i) = Pictures $ map (drawCurveSegment psd) $ traceCurve psd i
--    drawCurve psd (D i) = Color red $ glossify $ Bezier $ map ((psd ^.) . locationOf) $ curveVerts psd i
    drawCurve psd _     = Blank
    drawCurveSegment psd i = curveDrawEdge psd (_arc i) (psd ^. dataOf i, psd ^. dataOf (twin i)) $ _core $ edgeSegment i psd


curveDrawEdge :: EdgeDrawer s v e f r
curveDrawEdge _ i e s = Color red $ glossify $ dilate s 2



selecDrawVert :: VertDrawer s v e f r
selecDrawVert _ i v p = Color orange $ uncurry Translate (glossify p) $ circleSolid 17

selecDrawEdge :: EdgeDrawer s v e f r
selecDrawEdge _ i e s = Color orange $ glossify $ dilate s 7

selecDrawFace :: FaceDrawer s v e f r
selecDrawFace _ i f p = Color orange $ glossify $ erode p 7


selectable :: RealFrac r => Behaviour s v e f r
selectable = (drawState %~ (\f st -> drawOver (f st) $ Pictures $ map (drawSelection $ _subdivision st) $ _selection st))
           . (handleEvent %~~ handleSelection)
--  { drawState   = \s -> Pictures $ (map (drawSelection $ subdivision s) $ selection s) ++ [drawState app s]
--  , handleEvent = \e -> handleSelection e . handleEvent app e
--  , passTime    = passTime app
--  }
  where
    drawSelection :: RealFrac r => PlanarSubdivision s v e f r -> PSE s -> Picture
    drawSelection psd (V i) = selecDrawVert psd i (psd ^. dataOf i) (psd ^. locationOf i)
    drawSelection psd (D i) = selecDrawEdge psd (_arc i) (psd ^. dataOf i, psd ^. dataOf (twin i)) $ _core $ edgeSegment i psd
    drawSelection psd (F i) = selecDrawFace psd i (psd ^. dataOf i) $ _core $ rawFacePolygon i psd
    drawSelection psd N     = Blank

    handleSelection (EventKey (MouseButton LeftButton) Down _ _) s
      | _hover s == N                = s & selection .~ []
      | _hover s `elem` _selection s = s & selection %~ filter (/= _hover s)
      | otherwise                    = s & selection %~ (_hover s :)
    handleSelection _ s = s





-- locate things by mouse
-- simple implementation: function computing distance to object, minimize? (for faces take center? constant value inside (offset)?)


locate :: RealFrac r => PlanarSubdivision s v e f r -> Point 2 r -> PSE s
locate psd p = head $ (++ [N]) $ filter (/= N) 
  $  map (vertexScore p) (toList $ vertices psd)
  ++ map (edgeScore p) (toList $ edgeSegments psd)
  ++ map (faceScore p) (toList $ rawFacePolygons psd)

-- should make efficient implementation for point location - note this is a special kind that dilates vertices and edges

vertexScore :: (Num r, Ord r) => Point 2 r -> (VertexId' s, VertexData r v) -> PSE s
vertexScore p (i, VertexData q _) | d < 15^2  = V i
                                  | otherwise = N
  where d = squaredEuclideanDist p q

edgeScore :: RealFrac r => Point 2 r -> (Dart s, LineSegment 2 v r :+ e) -> PSE s
edgeScore p (i, s :+ _) | insidePolygon p $ dilate s 5 = D i
                        | otherwise                    = N

faceScore :: RealFrac r => Point 2 r -> (FaceId' s, SomePolygon v r :+ f) -> PSE s
faceScore p (i, q :+ _) | either (insidePolygon p) (insidePolygon p) q = F i
                        | otherwise         = N

