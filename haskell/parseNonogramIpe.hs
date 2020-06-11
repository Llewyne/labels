module ParseNonogramIpe where

import Control.Lens

import Data.Geometry hiding (head)
import Data.Geometry.PlanarSubdivision
import Data.Geometry.Ipe

-- data Options = Options { kind    :: String
--                        , inPath  :: FilePath
--                        , outPath :: FilePath
--                        }
--                deriving Data
data Options = Options { outPath :: FilePath} deriving Data


options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Parses a nonograms to ipe files"
               <> header   "Nonogram writer"
               )
  where
    parser = Options
          -- <$> strOption (help "Kind of input data in the input files" )
          -- <*> strOption (help "Input Directory")
          -- <*> strOption (help "Output Directory")
          <$> strOption (help "Output Directory")

--------------------------------------------------------------------------------

-- read a bunch of text files, each defining a time-series (ensemble), produce
-- an ipe file where each time-series is represented by a polyline.

main :: IO ()
main = execParser options >>= mainWith

mainWith                               :: Options -> IO ()
mainWith (Options outPath) = do
    let nonogram' = writeNonogram test
    writeIpeFile outPath . singlePageFromContent . map iO' $ nonogram'

writeNonogram :: [Label] -> IO()
