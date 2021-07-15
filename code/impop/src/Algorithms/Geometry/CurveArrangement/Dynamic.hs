module CurveArrangement.Dynamic where

import Prelude hiding (reverse)

import Control.Lens
import Data.Default
import Data.Foldable (toList)
import Data.Ext

import Data.Geometry.Point
import Data.Geometry.Vector hiding (head, init)
import Data.Geometry.BezierSpline hiding (endPoints)
import Data.Geometry.PlanarSubdivision
import Data.Geometry.PlanarSubdivision.Dynamic
import Data.Geometry.PlanarSubdivision.ForceDirected
import Data.Geometry.PlanarSubdivision.More

import Data.Geometry hiding (endPoints, head, init)

import Algorithms.Geometry.Misc

import CurveArrangement.Types

-- split an edge


-- TODO: do not split the constrained Bezier (or generally, Region)
-- rather, inherit constrained status from edge style - hmmmm
-- should edges know that they are part of a larger "input curve"?
subdivideEdge :: (Show r, RealFrac r) => Dart CAS -> r -> CA r -> CA r
subdivideEdge i t psd = 
  let (a, b) = psd & endPoints i
      al = psd ^. locationOf a & traverse %~ realToFrac
      bl = psd ^. locationOf b & traverse %~ realToFrac
      u = psd ^. dataOf i . stub
      v = psd ^. dataOf (twin i) . stub
      bez = Bezier3 al (al .+^ u) (bl .+^ v) bl
      p = evaluate bez (realToFrac t) & traverse %~ realToFrac
      --p = average [psd ^. locationOf a, psd ^. locationOf b]
      (bez1, bez2) = split (realToFrac t) bez
      vdata | psd ^. dataOf i . froz = def & fluid .~ Constrained bez
      -- possibly we want to check if a and b are already constrained to a superbez
            | otherwise = def
      f x | x == psd ^. dataOf i        = (x & stub .~ tangent bez1, x & stub .~ tangent bez2)
          | x == psd ^. dataOf (twin i) = (x & stub .~ tangent (reverse bez2), x & stub .~ tangent (reverse bez1)) 
          | otherwise = error "What the data?"
  in splitEdge a b p vdata f psd


-- mergeEdges :: (Show r, RealFrac r) => Dart CAS -> Dart CAS -> Maybe (BezierSpline 3 2 r) -> CA r -> CA r
-- mergeEdges i j mb ca | headOf i ca /= tailOf j ca = error "mergeEdges: can't merge these edges, man"
-- mergeEdges i j mb ca = 
--   let [a]        = common ca i j
--       f (e1, e2) | e1 == ca ^. dataOf i        = case mb of Just b  -> e1 & stub .~ (traverse %~ realToFrac $ tangent b)
--                                                             Nothing -> e1
--                  | e1 == ca ^. dataOf (twin j) = case mb of Just b  -> e1 & stub .~ (traverse %~ realToFrac $ tangent $ reverse b)
--                                                             Nothing -> e1
--                  | otherwise = error "What the data?"
--   in unSplitEdge a f ca


-- insert a new edge

insertEdge :: (Show r, Real r) => VertexId' CAS -> VertexId' CAS -> Maybe (BezierSpline 3 2 r) -> CA r -> CA r
insertEdge i j mb ca | i == j = ca
                     | length (commonDarts ca i j) > 0  = ca
                     | length (commonFaces ca i j) == 0 = ca
                     | otherwise = let f x = (x, x)
                                       il = ca ^. locationOf i & traverse %~ realToFrac
                                       jl = ca ^. locationOf j & traverse %~ realToFrac
                                       e1 = case mb of Just b  -> def & stub .~ (traverse %~ realToFrac $ tangent b)
                                                       Nothing -> def & stub .~ (jl .-. il) ^/ 3
                                       e2 = case mb of Just b  -> def & stub .~ (traverse %~ realToFrac $ tangent $ reverse b)
                                                       Nothing -> def & stub .~ (il .-. jl) ^/ 3
                                   in splitFace i j (e1, e2) f ca

-- dangleVertex :: (Show r, Real r) => VertexId' CAS -> FaceId' CAS -> Point 2 r -> BezierSpline 3 2 r -> CA r -> CA r
-- dangleVertex i j p b ca = 
--   let v = def
--       e1 = def & stub .~ (traverse %~ realToFrac $ tangent           b)
--       e2 = def & stub .~ (traverse %~ realToFrac $ tangent $ reverse b)
--   in sproutIntoFace i j p v (e1, e2) ca


{-
sproutIntoFace
  :: (Show v, Show e, Show f, Show r)
  => VertexId' s 
  -> FaceId' s 
  -> Point 2 r 
  -> v                       
  -> (e, e)
  -> PlanarSubdivision s v e f r 
  -> PlanarSubdivision s v e f r
-}

-- ^ these functions aren't used yet, obviously
