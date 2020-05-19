{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}


module Nonogram where

import Control.Lens

import Data.Geometry
import Data.Geometry.PlanarSubdivision


data CAS = CAS

type Frame    = SimplePolygon () Float
type Interior = PlanarSubdivision CAS () () Bool Float
type Clue     = [Int]

data Port = Port 
  { _location  :: Point 2 Float
  , _direction :: Vector 2 Float
  , _side      :: Bool
  } deriving (Show)
$(makeLenses ''Port)

data Label = Label
  { _clue      :: Clue
  , _port      :: Port
  , _offset    :: Float
  } 
$(makeLenses ''Label)

data Nonogram = Nonogram
  { _frame     :: Frame
  , _interior  :: Interior
  , _labels    :: [Label]
  }
$(makeLenses ''Nonogram)

type UnplacedLabel = ([Port], Clue)

placeLabels :: Frame -> [UnplacedLabel] -> [Label]
placeLabels _ = map placeLabelNaive

placeLabelNaive :: UnplacedLabel -> Label
placeLabelNaive ([]   , _) = error "placeLabelNaive: no available ports"
placeLabelNaive (p : _, c) = Label c p 0