{-# LANGUAGE TemplateHaskell #-}

module CurveArrangement.Types where


import Control.Lens
import Data.Default

import Data.Geometry.PlanarSubdivision
import Data.Geometry.PlanarSubdivision.ForceDirected
import Data.Geometry hiding (endPoints, head)

import Nonogram.PathType

data CAS = CAS

-- might want to restrict constrained vertices to cubic Beziers
-- maybe not in the ForceDirected module, but only here?

data CAV = CAV
  { _fl :: Fluidity
  , _fv :: ForceVector
  } deriving (Show, Eq)
$(makeLenses ''CAV)

instance Default CAV where def = CAV Fluid (origin .-. origin)

instance HasForceData CAV where
  fluid = fl
  force = fv

data CAE = CAE
  { _froz :: Bool
  , _stub :: Vector 2 Float -- would prefer :: Vector 2 r ?
  , _nonoe :: NonoPathType
  } deriving (Show, Eq)
$(makeLenses ''CAE)

instance Default CAE where def = CAE False (origin .-. origin) def

data CAF = CAF
  { _full :: Bool
  } deriving (Show, Eq)
$(makeLenses ''CAF)

instance Default CAF where def = CAF False

type CA r = PlanarSubdivision CAS CAV CAE CAF r

