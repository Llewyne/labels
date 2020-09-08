{-# LANGUAGE OverloadedStrings #-}

module SATLabelingTest where

import Control.Lens
import Data.Ext
import qualified Data.List.NonEmpty as NonEmpty

import Data.Geometry.Ipe

import Data.Geometry.Point
import Data.Geometry.BezierSpline hiding (snap)

import Data.Geometry.Polygon
import Data.Geometry.PlanarSubdivision
import Data.Geometry.PlanarSubdivision.More


import Algorithms.Geometry.Misc

import Algorithms.Geometry.EuclideanMST.EuclideanMST

import CurveArrangement
import CurveArrangement.Types
import CurveArrangement.Construct
import CurveArrangement.Ipe
import CurveArrangement.Puz

import Misc.Time
import Misc.Ipe

import Nonogram
import Nonogram.Puz
import Nonogram.Ipe
import Nonogram.PathType


import Misc.SpanningTree

import Eva.SATLabeling

testStar = labelTest "star"

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
  putStrLn $ "intersection points: " ++ show ((sum $ map length points))
  writeFile "log/points.txt" $ unlines $ map show points
  since start

  -- chop the curves into smaller pieces so they no longer intersect
  let chopped = concat $ zipWith subdivide curves points
  putStrLn $ "chopped curves: " ++ show (length chopped)
  writeFile "log/chopped.txt" $ unlines $ map show chopped
  since start

  -- chop the curves into smaller pieces so they no longer intersect
  let snapped = snap chopped
  putStrLn $ "snapped curves: " ++ show (length snapped)
  writeFile "log/snapped.txt" $ unlines $ map show snapped
  since start

  -- get collection of line segments
  let segs = map (bimap (const ()) id) $ map _core $ map seg snapped
  putStrLn $ "segments: " ++ show (length segs)
  writeFile "log/segments.txt" $ unlines $ map show segs
  since start

  -- construct an arrangement from the chopped curves
  let car = constructPlanar snapped
  putStrLn $ "curve arrangement:" 
  writeFile "log/car.txt" $ show car
  since start

  -- make a DFS tree of the faces of the arrangement
  let tre = faceTree car
  putStrLn $ "depth first tree: " ++ show (length tre) 
  writeFile "log/tree.txt" $ show tre
  since start

  -- find out which cells of the arrangement should be filled
  let caf = fillFaces car
  putStrLn $ "curve arrangement with solution:" 
  writeFile "log/caf.txt" $ show caf
  since start

  -- extract a list of pointers to faces that form solution
  let sol = fullFaces caf
  putStrLn $ "solution faces: " ++ show (length sol)
  writeFile "log/solution.txt" $ unlines $ map show sol
  since start

  -- collect clues in unplaced labels
  let upl = determineClues caf sol
  putStrLn $ "unplaced labels: " ++ show (length upl)  
  writeFile "log/unlabels.txt" $ unlines $ map show upl
  since start

  -- build the frame
  let frame = extractFrame caf
  putStrLn $ "frame: " ++ show (length $ polygonVertices frame) 
  writeFile "log/frame.txt" $ show frame
  since start

  -- place the labels
  pl <- placeLabelsSAT upl
  putStrLn $ "labels: " ++ show (length pl) 
  writeFile "log/labels.txt" $ unlines $ map show pl
  since start

  -- construct a nonogram
  let non = Nonogram frame caf pl
  putStrLn $ "nonogram: "
  writeFile "log/nonogram.txt" $ show non
  since start

  -- output the nonogram as ipe file
  let ipe = toIpe non
  putStrLn $ "ipe:"
  writeIpePage ("ipe/" ++ name ++ ".labeled.ipe") ipe
  since start

  -- output the nonogram as puzcode
  let puz = toPuzCode non
  putStrLn $ "puzcode:"
  writeFile ("puz/" ++ name ++ ".puz") puz
  since start

  
  return ()

