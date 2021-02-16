module Misc.Time (time, since) where

import System.Clock
import qualified Control.Exception as Exception


-- time :: IO GHC.Int.Int64
time = fmap sec $ getTime Monotonic

-- since :: GHC.Int.Int64 -> IO ()
since start = do
  now <- fmap sec $ getTime Monotonic
  putStrLn $ "TIME ELAPSED: " ++ showDuration (now - start)

showDuration :: (Integral a, Show a) => a -> String
showDuration n | n < 60    = show n ++ " seconds"
               | n < 3600  = show (n `div` 60   ) ++ " minutes and " ++ showDuration (n `mod` 60   )
               | n < 86400 = show (n `div` 3600 ) ++ " hours, "      ++ showDuration (n `mod` 3600 )
               | otherwise = show (n `div` 86400) ++ " days, "       ++ showDuration (n `mod` 86400)
