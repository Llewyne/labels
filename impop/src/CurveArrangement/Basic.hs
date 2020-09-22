
module CurveArrangement.Basic where


import Control.Lens
import Data.Default
import Data.Foldable (toList)
import Data.Ext


import Data.Geometry.BezierSpline
import Data.Geometry.LineSegment
import Data.Geometry.PlanarSubdivision hiding (endPoints)

import Data.Geometry hiding (endPoints, head, init)

import CurveArrangement.Types

edgeBezier :: (RealFrac r) => CA r -> Dart CAS -> BezierSpline 3 2 r
edgeBezier ca i = let p1 = ca ^. locationOf (tailOf i ca)
                      p4 = ca ^. locationOf (headOf i ca)
                      p2 = p1 .+^ (traverse %~ realToFrac) (_stub $ ca ^. dataOf i) 
                      p3 = p4 .+^ (traverse %~ realToFrac) (_stub $ ca ^. dataOf (twin i)) 
                  in Bezier3 p1 p2 p3 p4
