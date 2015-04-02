-- file reader.hs
-- Contains functions for reading input
-- and translating to data stuctures
import qualified Data.Vector as V
-- types are placeholder. MVector is more complex
type Lattice = V.Vector (Integer, [Double]) -- type and list of coords
type StateArr = V.Vector (V.Vector Integer) -- The state array
type AdjList = V.Vector [Integer]
-- takes a read text file and returns configuration lines
configFind :: String -> [String]
configFind input = takeWhile (/=footer) $ tail $ dropWhile (/=header) $ lines input where
  header = "[begin config]"
  footer = "[end config]"

--makeLattice :: [String] -> Lattice
--makeLattice x = V.fromList $ theList
latvars :: [String] -> [String]
latvars = map (fst.break (==')'))
tupler :: [a] -> [b] -> [(a,b)]
tupler = zip siteType coords where
  siteType = mapReadInt siteTypeChars where
    siteTypeChars = map (fst.break (==' ')) $ map (fst.break (=='(')) latvars
  coords   =  coordsChars where
    coordsChars   = map (drop 1) $ map (snd.break (=='(')) latvars

mapReadInt :: [String] -> [Int]
mapReadInt = map read
mapReadDouble :: [String] -> [Double]
mapReadDouble = map read
