{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}


module Nonogram where

import Control.Lens

import Data.Foldable (toList)
import Data.List

import Data.Geometry hiding (head)
import Data.Geometry.PlanarSubdivision
import Data.Geometry.PlanarSubdivision.More


import CurveArrangement
import CurveArrangement.Types


type Frame    = SimplePolygon () Float
type Interior = CA Rational -- change to Float!
type Clue     = [Int]

data Port = Port
  { _location  :: Point 2 Float
  , _direction :: Vector 2 Float
  , _side      :: Bool -- True if on right of direction vector (left if going into puzzle)
  }deriving (Show,Eq)
$(makeLenses ''Port)

data Label = Label
  { _clue      :: Clue
  , _port      :: Port
  , _offset    :: Float
  } deriving (Show,Eq)
$(makeLenses ''Label)

data Nonogram = Nonogram
  { _frame     :: Frame
  , _interior  :: Interior
  , _labels    :: [Label]
  } deriving Show
$(makeLenses ''Nonogram)

type UnplacedLabel = ([Port], Clue)

placeLabels :: Frame -> [UnplacedLabel] -> [Label]
placeLabels _ = map placeLabelNaive

placeLabelNaive :: UnplacedLabel -> Label
placeLabelNaive ([]   , _) = error "placeLabelNaive: no available ports"
placeLabelNaive (p : _, c) = Label c p 16



-- solve :: Interior -> Interior

type Solution = [FaceId' CAS]

determineClues :: Interior -> Solution -> [UnplacedLabel]
determineClues ca fs = map (determineClue ca fs) $ entryDarts ca

determineClue :: RealFrac r => CA r -> Solution -> Dart CAS -> UnplacedLabel
determineClue ca fs d = let curve = traceCurve ca d
                            faces = removeDuplicates $ leftFaces ca curve
                            sp = (traverse %~ realToFrac) $ ca ^. locationOf (tailOf (head curve) ca)
                            ep = (traverse %~ realToFrac) $ ca ^. locationOf (headOf (last curve) ca)
                            sv = (-1) *^ signorm (_stub $ ca ^. dataOf (head curve))
                            ev = (-1) *^ signorm (_stub $ ca ^. dataOf (twin $ last curve))
                        in ([Port sp sv True, Port ep ev False], extractClue faces fs)

entryDarts :: CA r -> [Dart CAS]
entryDarts ca = let vs = toList $ boundaryVertices (outerFaceId ca) ca
                    es = concatMap (\v -> filter (\e -> tailOf e ca == v) $ incidences ca v) vs
                in filter (\e -> leftFace e ca /= outerFaceId ca && rightFace e ca /= outerFaceId ca) es

-- traceCurve :: PlanarSubdivision s v e f r -> Dart s -> [Dart s]


leftFaces :: PlanarSubdivision s v e f r -> [Dart s] -> [FaceId' s]
leftFaces = map . flip leftFace

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = map head . group

extractClue :: Eq a => [a] -> [a] -> [Int]
extractClue xs sol = check0 $ map length $ filter or $ group $ map (`elem` sol) xs

check0 :: [Int] -> [Int]
check0 [] = [0]
check0 xs = xs

{- not necessary: this is Data.List.group
clump :: Eq a => [a] -> [[a]]
clump [] = []
clump [x] = [[x]]
clump (x : y : xs) | x /= y = [x] : (clump $ y : xs)
                   | x == y = (x : head (clump $ y : xs)) : tail (clump $ y : xs)
-}



