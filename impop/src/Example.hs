{-# LANGUAGE OverloadedStrings #-}

module LabelTest where

import Control.Lens
import Data.Ext
import qualified Data.List.NonEmpty as NonEmpty

import Data.Geometry.Ipe

import Data.Geometry.Point
import Data.Geometry.BezierSpline hiding (snap)

import Data.Geometry.PlanarSubdivision.More

import Algorithms.Geometry.Misc

import Algorithms.Geometry.EuclideanMST.EuclideanMST

import CurveArrangement.Types
import CurveArrangement.Construct
import CurveArrangement.Ipe
import CurveArrangement.Puz

import Misc.Time
import qualified Control.Exception as Exception



import Nonogram
import Nonogram.Puz




labelTest :: String -> IO ()
labelTest name = do

  -- get the current time
  start <- time
  since start

  -- first, read puzzle as a raw ipe page
  page <- readIpePage $ "ipe/" ++ name ++ ".ipe"

  -- extract the contents of the page as a set of curves
  let curves = convertPathsToBeziers $ pagePaths page
  putStrLn $ "converted curves: " ++ show (length curves)
  writeFile "log/converted.txt" $ unlines $ map show curves
  since start

  -- compute all intersection points between the curves
  let points = intersectionPoints $ map _core curves
  putStrLn $ "intersection points: " ++ show ((sum $ map length points) `div` 2)
  writeFile "log/points.txt" $ unlines $ map show points
  since start

  -- chop the curves into smaller pieces so they no longer intersect
  let chopped = concat $ zipWith subdivide curves points
  putStrLn $ "chopped curves: " ++ show (length chopped)
  writeFile "log/chopped.txt" $ unlines $ map show chopped
  since start

  -- construct an arrangement from the chopped curves
  let car = constructPlanar chopped
  putStrLn $ "curve arrangement:" 
  writeFile "log/car.txt" $ show car
  since start

  -- find out which cells of the arrangement should be filled
  let caf = findFilled car
  putStrLn $ "curve arrangement with solution:" 
  writeFile "log/caf.txt" $ show caf
  since start

  -- collect clues in unplaced labels
  let upl = determineClues caf sol
  putStrLn $ "unplaced labels: " ++ show (length upl)  
  writeFile "log/unlabels.txt" $ unlines $ map show upl
  since start

  -- place the labels
  let pl  = placeLabels (frame car) upl
  putStrLn $ "labels: " ++ show (length pl) 
  writeFile "log/labels.txt" $ unlines $ map show pl
  since start

  -- construct a nonogram
  let non = Nonogram undefined caf pl
  putStrLn $ "nonogram: "
  writeFile "log/nonogram.txt" $ show non
  since start

  -- output the nonogram as puzcode
  let puz = toPuzCode non
  putStrLn $ "puzcode:"
  writeFile ("puz/" ++ name ++ ".puz") puz
  since start

  -- output the nonogram as ipe file
  let ipe = toIpe non
  putStrLn $ "ipe:"
  writeIpePage ("ipe/" ++ name ++ ".labeled.ipe") ipe
  since start
  
  return ()














