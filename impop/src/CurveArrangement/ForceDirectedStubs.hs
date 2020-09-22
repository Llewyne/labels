
module CurveArrangement.ForceDirectedStubs where

import CurveArrangement.Types


import Control.Lens
import Data.Foldable (toList)

import Data.Geometry.PlanarSubdivision
import Data.Geometry.PlanarSubdivision.ForceDirected
import Data.Geometry.PlanarSubdivision.More

import Data.Geometry hiding (endPoints, head)

import PSDGlossApp.Common




-- functionality for making stubs auto-update after forcible:

forcibleStubs :: RealFrac r => Behaviour CAS CAV CAE CAF r
forcibleStubs = passTime %~~ (\d -> (subdivision %~ smoothStubs))

smoothStubs :: RealFrac r => CA r -> CA r
smoothStubs psd = foldr ($) psd $ fmap smoothStubsV $ vertices' psd

smoothStubsV :: RealFrac r => VertexId' CAS -> CA r -> CA r
smoothStubsV i psd | even $ length $ neighboursOf i psd = smoothStubsE i psd
                   | otherwise = smoothStubsO i psd
  
smoothStubsE :: RealFrac r => VertexId' CAS -> CA r -> CA r
smoothStubsE i psd =
  let pairs = opposites $ toList $ neighboursOf i psd
  in foldr (applyStub i) psd $ concatMap (straighten psd) pairs

smoothStubsO :: RealFrac r => VertexId' CAS -> CA r -> CA r
smoothStubsO i psd = foldr (applyStub i) psd $ fmap (point psd i) $ neighboursOf i psd

applyStub :: RealFrac r => VertexId' CAS -> (VertexId' CAS, Vector 2 Float) -> CA r -> CA r
applyStub i (j, v) psd = case psd ^. dataOf i . fluid of
    Fixed p     -> psd
    _           -> applyStubE (dartFromTo psd i j) v psd
    -- same for Fluid and Contrained?

applyStubE :: RealFrac r => Dart CAS -> Vector 2 Float -> CA r -> CA r
applyStubE e v psd | psd ^. dataOf e . froz = psd
                   | otherwise = psd & dataOf e . stub .~ v

straighten :: Real r => PlanarSubdivision s v e f r -> (VertexId' s, VertexId' s) -> [(VertexId' s, Vector 2 Float)]
straighten psd (i, k) = let v = traverse %~ realToFrac $ (psd ^. locationOf k .-. psd ^. locationOf i)
                        in [(i, (-0.17) *^ v), (k, (0.17) *^ v)]

point :: Real r => PlanarSubdivision s v e f r -> VertexId' s -> VertexId' s -> (VertexId' s, Vector 2 Float)
point psd i j = let v = traverse %~ realToFrac $ (psd ^. locationOf j .-. psd ^. locationOf i)
                in (j, (0.33) *^ v)

opposites :: [a] -> [(a, a)]
opposites ls | even $ length ls = zip ls $ drop (length ls `div` 2) ls
             | otherwise = []

dartFromTo :: PlanarSubdivision s v e f r -> VertexId' s -> VertexId' s -> Dart s
dartFromTo psd i j 
  | length (commonDarts psd i j) == 0 = error "dartFromTo: not adjacent"
  | otherwise = head $ filter (\e -> tailOf e psd == i) $ commonDarts psd i j
