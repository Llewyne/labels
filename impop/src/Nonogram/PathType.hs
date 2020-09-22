{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}


module Nonogram.PathType where

import Control.Lens
import Data.Default
import Data.Foldable (toList)
import Data.List

--import Nonogram

data NonoPathType = NonoPathType
  { _framePath    :: Bool -- True if part of the outer frame of the puzzle
  , _boundaryPath :: Bool -- True if part of the image boundary
  } deriving (Eq, Show)
$(makeLenses ''NonoPathType)

isFixed :: NonoPathType -> Bool
isFixed p = _framePath p || _boundaryPath p

instance Default NonoPathType where def = NonoPathType False False