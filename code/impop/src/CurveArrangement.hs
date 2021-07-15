module CurveArrangement where


import Control.Lens
import Data.Default
import Data.Foldable (toList)
import Data.Ext

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Geometry.Point
import Data.Geometry.Vector hiding (head, init)
import Data.Geometry.BezierSpline
import Data.Geometry.PlanarSubdivision
import Data.Geometry.PlanarSubdivision.Dynamic
import Data.Geometry.PlanarSubdivision.ForceDirected
import Data.Geometry.PlanarSubdivision.More

import Data.Geometry hiding (endPoints, head, init)
import qualified Data.CircularSeq as C

import Graphics.Gloss hiding (Point, Vector, Path, Arc, arc, display)
import Graphics.Gloss.Interface.Pure.Game hiding (Point, Vector, Path, Arc, arc)

import Glossify

import PSDGlossApp
import PSDGlossApp.Common
import PSDGlossApp.Visible

import Data.Geometry.Ipe
import Data.Geometry.Ipe.Path

import Algorithms.Geometry.Misc

import CurveArrangement.Types
import CurveArrangement.ForceDirectedStubs
import CurveArrangement.Ipe
import CurveArrangement.Puz
import CurveArrangement.Loader
import CurveArrangement.Basic
import CurveArrangement.Dynamic


-- move a contrained vertex:
  -- re-compute its location based on its constrained Bezier
  -- update tangent vectors on neighbouring vertices
  -- if multiple constrained vertices on the same Bezier: how do we do this?
    -- ^ use parameters on curve and "subcurve"


drawEdgeCurve :: Show r => EdgeDrawer CAS CAV CAE CAF r
drawEdgeCurve _ _ (e1, e2) s = Color black $ glossify $ edgeDataBezier (e1, e2) s






editable' :: (Show r, RealFrac r) => Behaviour CAS CAV CAE CAF r
editable' = handleEvent %~~ handleEdit
  where
    handleEdit (EventKey (SpecialKey KeySpace) Down _ _) s = -- traceShow (selection s) $
      let l = _selection s
      in case l of [D i]      -> s & subdivision %~ (subdivideEdge i 0.5)
                   [V a, V b] -> s & subdivision %~ (insertEdge a b Nothing)
                   _          -> s
    handleEdit _ s = s







-- need:


-- select, show (color), etc. complete curves based on opposite edges

-- More.hs 
-- traceCurve :: PlanarSubdivision s v e f r -> Dart s -> [Dart s]


-- better force functions that guarantee planarity (perhaps the guarantee should be separated from the user-defined force)

-- make vertices partially fixed on an *edge*


-- trace a path from a (boundary) edge through cells, creating new vertices and edges





it :: IO ()
it = do
  p <- readSinglePageFile "ipe/note-converted.ipe"
  case p of
      Left err                         -> print err
      Right (page :: IpePage Rational) -> ipeTest page
      -- writeIpeFile "bla.out" . singlePageFromContent $ page ^. content

ipeTest :: (Ord r, Enum r, RealFrac r, Show r) => IpePage r -> IO ()
ipeTest page = run ( id
                   . forcibleStubs
                   . forcible (simpleForces +++ curvedForces) 
--                   . annotVisible 
                   . drawEdgesWith drawEdgeCurve
                   . basicVisible
                   ) $ fromIpePage page

{-  
instance IpeWriteText Float where
  ipeWriteText = writeByShow

instance Coordinate Float where
-}






main :: IO ()
main = run ( id
--           . traceable
           . forcibleStubs
           . forcible (simpleForces +++ curvedForces) 
           . editable'
           . selectable 
           . hoverable . hoverableCurves
--           . annotVisible 
           . drawEdgesWith drawEdgeCurve
           . basicVisible
           ) $ fromFrame vierkant


-- initialize a curve arrangement


--dartData :: Lens (PlanarSubdivision s v e f r) (PlanarSubdivision s v e' f r) (Vector (Dart s, e)) (Vector (Dart s, e'))

fromFrame :: RealFrac r => SimplePolygon CAV r -> CA r
fromFrame p = id
        $ applyAll fixEdge -- fix vertices with "fixall"
        $ applyAll fixVertex -- fix vertices with "fixall"
        $ (dartData . traverse %~ (\(d, ()) -> (d, def)))
        $ fromPolygon (Identity CAS) p def def

fixEdge :: Real r => Dart s -> PlanarSubdivision s v CAE f r -> PlanarSubdivision s v CAE f r
fixEdge i psd = psd & dataOf i . froz .~ True
                    & dataOf i . stub .~ v ^/ 3
  where v =   (traverse %~ realToFrac $ psd ^. locationOf (headOf i psd))
          .-. (traverse %~ realToFrac $ psd ^. locationOf (tailOf i psd))


fixVertex :: (HasForceData v, Real r) => VertexId' s -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
fixVertex i psd = psd & dataOf i . fluid .~ Fixed (traverse %~ realToFrac $ psd ^. locationOf i)


vierkant = simpleFromPoints . map (:+ def)
         $ [ Point2 (-250) (-250)
--       , Point2 0 (-250)
         , Point2 250 (-250)
--       , Point2 250 0
         , Point2 250 250
--       , Point2 0 250
         , Point2 (-250) 250
--       , Point2 (-250) 0
         ]











extractFrame :: Real r => CA r -> SimplePolygon () Float
extractFrame ca = bimap (const ()) (realToFrac) $ _core $ rawFaceBoundary (outerFaceId ca) ca 

