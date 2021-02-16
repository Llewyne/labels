{-# LANGUAGE OverloadedStrings #-}

module Test where

import Control.Lens
import Data.Ext
import qualified Data.List.NonEmpty as NonEmpty

import Data.Default

import Data.Geometry.Ipe
import Data.Geometry.Ipe.Types
import Data.Geometry.Ipe.Color (named)

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
import Misc.Ipe

import Misc.SpanningTree

import Nonogram
import Nonogram.Puz


redPaths :: [Path Float :+ IpeAttributes Path Float] -> [Path Float :+ IpeAttributes Path Float]
redPaths = filter (\p -> p^.extra.attrLens SStroke == Just red)
  where red = named "red" :: IpeColor Float






testcurve1 :: BezierSpline 3 2 Rational
testcurve1 = Bezier3 (Point2 (-250) (-250)) (Point2 (-100) (100)) (Point2 (100) (-100)) (Point2 (250) (250))

testcurve2 :: BezierSpline 3 2 Rational
testcurve2 = Bezier3 (Point2 (250) (-250)) (Point2 (-50) (100)) (Point2 (50) (-150)) (Point2 (-250) (250))


testcurve3, testcurve4 :: BezierSpline 3 2 Rational
testcurve3 = Bezier3 (Point2 (-128) (128)) (Point2 (48) (80)) (Point2 (-16) (-48)) (Point2 (96) (-64))
testcurve4 = Bezier3 (Point2 (-64) (-80)) (Point2 (-48) (32)) (Point2 (48) (48)) (Point2 (64) (96))


--promised:
--The test cases for P2 will be valid arms for the application. This means they may start with either a link or a joint, but will always end with a link. Every link will have a length between 0.1 and 0.5, and the final link will have a length of 0.1. If starting with a link, the first link will be vertical (in fact, this is not part of the arm description but hard-coded). An angle of 0 at a joint always means the two adjacent links will be collinear, and if the arm starts with a joint, an angle of 0 means vertical. In addition, the test cases for P2 will not have two consecutive links or joints. (For P3, you will design your own arm, which should also adhere to these length restrictions, but is allowed to have consecutive links or joints.)For P2, you may see the arms as free-standing; that is, a solution is valid if putting the joints at the given angles places the tip in the correct location, irrespective of the geometry of the resulting arm configuration. In particular, it will not be checked for collisions with the environment. (For P3, you will not be able to move arms through obstacles.)


makePlanar :: String -> IO ()
makePlanar name = do
  let input  = "ipe/" ++ name ++ ".ipe"
      output = "ipe/" ++ name ++ ".planar.ipe"
  page <- readIpePage input :: IO (IpePage Rational)
  let car = fromIpePage page
  writeIpePage output $ toIpePage car


ipeOutputCurvesWithPointSet :: [BezierSpline 3 2 Rational] -> [Point 2 Rational] -> IpePage Rational
ipeOutputCurvesWithPointSet curves points =
  let paths = map makePath $ map (:+ def) curves 
      marks = map ipeDiskMark points
  in fromContent $ map iO paths ++ map iO marks
  -- turn segments into paths


readPlanar :: String -> IO (CA Rational)
readPlanar name = do
  let input = "ipe/" ++ name ++ ".planar.ipe"
  page <- readIpePage input
  return $ fromIpePlanar page

writePuz :: String -> CA Rational -> IO ()
writePuz name car = do
  let output = "puz/" ++ name ++ ".puz"
--      sol = map toEnum [5, 10, 11, 13, 17, 18, 23]
      sol = map toEnum [1, 2, 3, 4, 6, 8, 10]
      upl = determineClues car sol
      pl  = placeLabels undefined upl  
      non = Nonogram undefined car pl
      cod = toPuzCode non
  putStrLn $ "puzcode:"
  writeFile output cod

fish :: IO ()
fish = do
  car <- readPlanar "fish-converted"
  writePuz "fish-converted" car

test :: IO ()
test = ipeTest "fox-converted"

ipeTest :: String -> IO ()
ipeTest name = do

  start <- time
  since start

  page <- readIpePage ("ipe/" ++ name ++ ".ipe")

  let curves = convertPathsToBeziers $ pagePaths page
  putStrLn $ "converted curves: " ++ show (length curves)
  writeFile "log/converted.txt" $ unlines $ map show curves
--  print $ intersectB (head curves) (last curves)
  since start


  let points = intersectionPoints $ map _core curves
  putStrLn $ "intersection points: " ++ show ((sum $ map length points) `div` 2)
  writeFile "log/points.txt" $ unlines $ map show points
  since start


  -- question is: are these points correct?
  -- seems maybe we find all interior intersection points, but not endpoints and endpoints-on-curves?
  writeIpePage "ipe/points.ipe" $ ipeOutputCurvesWithPointSet (map _core curves) $ concat points


--chopQuadratic = concat $ zipWith subdivide curves $ intersectionPoints $ map _core curves
  let chopped = concat $ zipWith subdivide curves points
  putStrLn $ "chopped curves: " ++ show (length chopped)
  writeFile "log/chopped.txt" $ unlines $ map show chopped
--  mapM_ print $ removeDuplicates $ collectEndpoints $ chopped
--  print $ euclideanMST $ NonEmpty.fromList $ map (:+ ()) $ removeDuplicates $ collectEndpoints $ chopped
  since start

{-
  let snapped = snap chopped
  putStrLn $ "snapped curves: " ++ show (length snapped)
  writeFile "log/snapped.txt" $ unlines $ map show $ snapped
  since start
-}

  let car = constructPlanar chopped
  putStrLn $ "curve arrangement:" 
  writeFile "log/car.txt" $ show car
  since start


  let ipeFile = singlePageFile $ toIpePage car
  writeIpeFile ("ipe/" ++ name ++ ".planar.ipe") ipeFile


  let caf = car -- convert to Float
      sol = map toEnum [5, 10, 11, 13, 17, 18, 23]

      ets = entryDarts caf
      cus = map (traceCurve caf) ets
      fas = map (leftFaces caf) cus
                            
      upl = determineClues caf sol
      pl  = placeLabels undefined upl

  putStrLn $ "labels: " ++ show (length pl) 
  writeFile "log/labels.txt" $ unlines 
    $    map show ets
      ++ map show cus
      ++ map show fas
      ++ map show pl
  since start


  let non = Nonogram undefined caf pl
      cod = toPuzCode non
  putStrLn $ "puzcode:"
  writeFile ("puz/" ++ name ++ ".puz") cod
  since start

  
  return ()




bla :: [Point 2 Rational]
bla = [ Point2 (-128) (-16)
      , Point2 (-64) (-80)
      , Point2 (-2097151243 / 32768000) (-2621440757 / 32768000)
      , Point2 (-16) (-128)
      , Point2 64 96
      ]

blaMST = euclideanMST $ NonEmpty.fromList $ map (:+ ()) $ bla

-- euclideanMST $ NonEmpty.fromList $ map (:+ ()) [ Point2 (-128) (-16)      , Point2 (-64) (-80)      , Point2 (-2097151243 / 32768000) (-2621440757 / 32768000)      , Point2 (-16) (-128)      , Point2 64 96      ]