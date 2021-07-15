{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module PP.Puz where


import Control.Lens
import Data.Foldable (toList)

import Data.Geometry.Point
import Data.Geometry.Vector hiding (head, init)
import Data.Geometry.BezierSpline
import Data.Geometry.LineSegment
import Data.Geometry.PlanarSubdivision hiding (endPoints)

import Data.Geometry hiding (endPoints, head, init)

--import Convert

import Algorithms.Geometry.Misc



makeDynamic :: PuzPath -> PuzObject
makeDynamic p = Dynamic (Path p) [Modify Solid $ Path p] [] -- need a way to mark empty faces, requires largest empty circle in polygon

bezierToPuz :: (RealFrac r, Show r) => BezierSpline 3 2 r -> PuzPath
bezierToPuz b = beziersToPuz [b]

beziersToPuz :: (RealFrac r, Show r) => [BezierSpline 3 2 r] -> PuzPath
beziersToPuz bs = pointsToPuzPath $ map (traverse %~ realToFrac) $ concatenate $ map (approximate resolution) bs
  where resolution = 1

concatenate :: Eq a => [[a]] -> [a]
concatenate = removeConsecutiveDuplicates . concat

removeConsecutiveDuplicates :: Eq a => [a] -> [a]
removeConsecutiveDuplicates []  = []
removeConsecutiveDuplicates [x] = [x]
removeConsecutiveDuplicates (x : y : xs) | x == y = removeConsecutiveDuplicates (x : xs)
                                         | otherwise = x : removeConsecutiveDuplicates (y : xs)

pointsToPuzPath :: [Point 2 Float] -> PuzPath
pointsToPuzPath [] = error "pointsToPuzPath: empty"
pointsToPuzPath (p : ps) = foldl LineTo (Start p) ps

translate :: RealFrac r => Vector 2 r -> PuzObject -> PuzObject
translate v = let [x, y] = map realToFrac $ toList v
              in Translate x y


data PuzObject = Path PuzPath
               | Rectangle Float Float Float Float
               | Compound [PuzObject]
               | Text String
               | Dynamic PuzObject [PuzObject] [PuzObject] -- DynamicElements have keys!
--               | Define
--               | Identifier               
--               | Structural PuzObject [PuzObject]
               | Modify PuzModifier PuzObject
               | Translate Float Float PuzObject

data PuzModifier = Solid | Thin | Thick | Transparent

data PuzPath = Start (Point 2 Float)
             | LineTo PuzPath (Point 2 Float)
             | ArcTo PuzPath Bool Bool (Point 2 Float)


class Puz p where
  code :: p -> String

instance Puz (Point 2 Float) where
--  code p = show (p ^. unsafeCoord 1) ++ " " ++ show (p ^. unsafeCoord 2)
  code p = unwords $ map (show . (flip view p) . unsafeCoord) [1, 2]

instance Puz PuzPath where
  code (Start p) = "  start " ++ code p
  code (LineTo path p) = code path ++ "\n  " ++ "line to " ++ code p
  code (ArcTo path left wide p) = code path ++ "\n  " ++ if left then "left " else "" 
                                                      ++ if wide then "wide " else ""
                                                      ++ "arc to " ++ code p 

instance Puz PuzModifier where
  code Solid       = "solid"
  code Thick       = "thick"
  code Thin        = "thin"
  code Transparent = "transparent"

instance Puz PuzObject where
  code (Path p)            = "path {\n" ++ code p ++ "\n}"
  code (Rectangle l b w h) = "rectangle " ++ unwords (map show [l, b, w, h])
  code (Compound ps)       = "compound {\n" ++ unlines (map code ps) ++ "\n}"
  code (Text t)            = "text " ++ show t
  code (Dynamic p ls rs)   = "dynamic " ++ code p ++ " {\n" ++ unlines (map code ls) ++ "\n}"
                                                  ++ " {\n" ++ unlines (map code rs) ++ "\n}"
  code (Modify m p)        = code m ++ " " ++ code p
  code (Translate x y p)   = "translate " ++ show x ++ " " ++ show y ++ " " ++ code p

-- make prettier layout?










