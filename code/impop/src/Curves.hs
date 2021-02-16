module CurveArrangement where

import Curves

data CAS = CAS

-- this should be in ForceDirected
data Fluidity = Fluid | Fixed (Point 2 r) | Constrained (Bezier 2 r)

data CAV
  { fluid :: Fluidity
  , :: ForceVector
  , 
  }

instance HasForceData VertexData where

data CAE
  { fluid :: Bool
  , tangent :: Vector 2 r
  , 
  }

data CAF = CAF

type CA r = PlanarSubdivision CAS CAV CAE CAF r


-- need:


-- select, show (color), etc. complete curves based on opposite edges

-- More.hs 
-- traceCurve :: PlanarSubdivision s v e f r -> Dart s -> [Dart s]


-- visualize edges as actual curves? (maybe: store direction vectors at each vertex-edge incidence, as if the were cubic Beziers?)

-- better force functions that guarantee planarity (perhaps the guarantee should be separated from the user-defined force)

-- make vertices partially fixed on an *edge*


-- trace a path from a (boundary) edge through cells, creating new vertices and edges
