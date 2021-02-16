module Misc.Ipe where

import Data.Geometry.Ipe
import Data.Geometry.Ipe.Types

readIpePage :: (Eq r, Coordinate r) => FilePath -> IO (IpePage r)
readIpePage filepath = do
  p <- readSinglePageFile filepath -- applies all matrices
  case p of
      Left err   -> error $ show err
      Right page -> return page

