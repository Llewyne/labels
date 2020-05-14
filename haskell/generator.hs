module Generator where

import System.Random
import Data.List
import Drawing
import Graphics.Gloss


w = 500.0
h = 500.0

amtSlopes = 5
amtLines = 20

gen = do
    seed1 <- newStdGen
   
    let (_ms,seed2) = randomFloats amtSlopes ((-0.5)*pi) (0.5*pi) seed1
    let ms = map tan (_ms)
    let (xs,seed3) = randomFloats amtLines 0 w seed2
    let (ys,seed4) = randomFloats amtLines 0 h seed3
    let (tops,seed5) = randomClues amtLines 1 5 0 5 seed4
    let (bottoms,seed6) = randomClues amtLines 1 5 0 5 seed5
    let ps = zip xs ys
    let rs = zip (concat $ repeat ms) ps
    let bs = zipWith getB (concat $ repeat ms) ps
    print $ rs
    print ms
    print bs
    print tops
    print bottoms
    display
         (InWindow
	       "Labels" 	 -- window title
		(floor w + 300, floor h + 300) 	 -- window size
		(100, 100)) 	 -- window position
	white			 -- background color
	(p (concat $ repeat ms) bs tops bottoms)		 -- picture to display


test = Pictures [(grid w h),nonoline w h 0.5 10 [1,2,3] [4,5,6]]

{-p ms bs = Translate (-(w/2)) (-(h/2)) $ Pictures $ zipWith (lineIn w h) ms bs-}
p ms bs tops bottoms = Pictures ((grid w h):(zipWith (\(a,b) (c,d)->nonoline w h a b c d) (zip ms bs) (zip tops bottoms)))


-- randomMs :: Int -> StdGen -> [Float]
-- randomMs n = take n . unfoldr (Just . randomR ((-0.5)*pi,0.5*pi))

-- randomXs :: Int -> StdGen -> [Float]
-- randomXs n = take n . unfoldr (Just . randomR (0, w))

-- randomYs :: Int -> StdGen -> [Float]
-- randomYs n = take n . unfoldr (Just . randomR (0, h))
randomFloats :: Int -> Float -> Float -> StdGen -> ([Float],StdGen)
randomFloats n start end ig = (take n  $ randomRs (start,end) ig, snd $ next ig)

randomInts :: Int -> Int -> Int -> StdGen -> ([Int],StdGen)
randomInts n start end ig = (take n  $ randomRs (start,end) ig, snd $ next ig)

randomClues :: Int -> Int -> Int ->  Int -> Int -> StdGen -> ([[Int]],StdGen)
randomClues n s1 e1 s2 e2 ig = (_r ns g, f)
    where 
        (g,f) = split ig
        ns = take n  $ randomRs (s2,e2) ig
        _r [] _ = []
        _r (n1:ns) g = i : _r ns newG
            where (i,newG) = randomInts n1 s1 e1 g
