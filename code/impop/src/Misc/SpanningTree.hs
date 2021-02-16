module Misc.SpanningTree where

import Data.Set hiding (foldr)
import Data.Tree

-- | Creates a spanning tree of an abstract graph, by performing a depth-first search

spanningTree :: Ord a => (a -> [a]) -> a -> Tree a
spanningTree neighbours root = head $ fst $ sTree neighbours root ([], empty)

sTree :: Ord a => (a -> [a]) -> a -> (Forest a, Set a) -> (Forest a, Set a)
sTree neighbours root (forest, used)
  | root `member` used = (forest, used)
  | otherwise          = let (newforest, newused) = foldr (sTree neighbours) ([], insert root used) (neighbours root)
                         in (Node root newforest : forest, newused)




test :: Int -> [Int]
test 0 = [3,4,1,2]
test 1 = [2,4,0]
test 2 = [0,1,3,5]
test 3 = [2,4,0]
test 4 = [0,1,5,3]
test 5 = [2,4]
test _ = []