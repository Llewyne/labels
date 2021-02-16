
module CurveArrangement.ForceDirectedStubs where

import CurveArrangement.Types


import Control.Lens
import Data.Foldable (toList)

import Data.Geometry.PlanarSubdivision
import Data.Geometry.PlanarSubdivision.ForceDirected
import Data.Geometry.PlanarSubdivision.More

import Data.Geometry hiding (endPoints, head)

import PSDGlossApp.Common



-- MUST HAVES
-- stubs that are opposite frozen edges should have fixed direction!

-- NICE TO HAVES
-- probably want a single force function that moves stubs and locations
-- should detect and delete subdivided edges that are too short


-- functionality for making stubs auto-update after forcible:

forcibleStubs :: RealFrac r => Behaviour CAS CAV CAE CAF r
forcibleStubs = passTime %~~ forceTime

forceTime :: RealFrac r => Float -> State CAS CAV CAE CAF r -> State CAS CAV CAE CAF r
forceTime d s | s ^. frozen = s
              | otherwise   = s & (subdivision %~ smoothStubs)


smoothStubs :: RealFrac r => CA r -> CA r
smoothStubs ca = foldr ($) ca $ fmap smoothStubsV $ vertices' ca

smoothStubsV :: RealFrac r => VertexId' CAS -> CA r -> CA r
smoothStubsV i ca | even $ length $ neighboursOf i ca = smoothStubsEven i ca
                  | otherwise = smoothStubsOdd i ca
  
smoothStubsEven :: RealFrac r => VertexId' CAS -> CA r -> CA r
smoothStubsEven i ca =
  let pairs = opposites $ toList $ neighboursOf i ca
  in foldr (applyStub i) ca $ concatMap (straighten ca) pairs

smoothStubsOdd :: RealFrac r => VertexId' CAS -> CA r -> CA r
smoothStubsOdd i ca = foldr (applyStub i) ca $ fmap (point ca i) $ neighboursOf i ca

-- applystub:
-- if vertex i is fluid: just do it
-- if vertex i is constrained: depends on vertex j
-- if vertex i is fixed: depends on vertex j as well
applyStub :: RealFrac r => VertexId' CAS -> (VertexId' CAS, Vector 2 Float) -> CA r -> CA r
applyStub i (j, v) ca = case ca ^. dataOf i . fluid of
--    Fixed p     -> ca
    _           -> applyStubEdge (dartFromTo ca i j) v ca
    -- same for Fluid and Contrained?

applyStubEdge :: RealFrac r => Dart CAS -> Vector 2 Float -> CA r -> CA r
applyStubEdge e v ca | isFrozen ca (Just e)                           = ca
                     | isFrozen ca (oppositeIncidentEdge ca $ twin e) = ca & dataOf e . stub .~ projectStub ca e v
                     | otherwise = ca & dataOf e . stub .~ v

projectStub :: CA r -> Dart CAS -> Vector 2 Float -> Vector 2 Float
projectStub ca e v =
  let Just op = oppositeIncidentEdge ca $ twin e
      base    = (-1) *^ (ca ^. dataOf op . stub)
  in (norm v / norm base) *^ base -- find length based on v, take care it's not negative

isFrozen :: CA r -> Maybe (Dart CAS) -> Bool
isFrozen ca Nothing  = False
isFrozen ca (Just i) = ca ^. dataOf i . froz

straighten :: Real r => PlanarSubdivision s v e f r -> (VertexId' s, VertexId' s) -> [(VertexId' s, Vector 2 Float)]
straighten ca (i, k) = let v = traverse %~ realToFrac $ (ca ^. locationOf k .-. ca ^. locationOf i)
                       in [(i, (-0.17) *^ v), (k, (0.17) *^ v)]

point :: Real r => PlanarSubdivision s v e f r -> VertexId' s -> VertexId' s -> (VertexId' s, Vector 2 Float)
point ca i j = let v = traverse %~ realToFrac $ (ca ^. locationOf j .-. ca ^. locationOf i)
               in (j, (0.33) *^ v)

opposites :: [a] -> [(a, a)]
opposites ls | even $ length ls = zip ls $ drop (length ls `div` 2) ls
             | otherwise = []

dartFromTo :: PlanarSubdivision s v e f r -> VertexId' s -> VertexId' s -> Dart s
dartFromTo ca i j 
  | length (commonDarts ca i j) == 0 = error "dartFromTo: not adjacent"
  | otherwise = head $ filter (\e -> tailOf e ca == i) $ commonDarts ca i j


