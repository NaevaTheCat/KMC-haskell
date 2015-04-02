--file KMC/importfuncs.hs
import qualified Data.Vector as V
-- placeholder function. Likely need to modify for MVectors
-- Lattice is the info about the vertexes of the graph. 
-- SiteType, [xcord,ycord,zcord]
type Lattice = V.Vector (Int,[Double])
type StateArray = V.Vector (V.Vector (Int))
type AdjList = V.Vector ([Int])
makeLattice :: String -> Lattice
makeLattice input = V.fromList $ zip (siteTypes.(configFind header footer) $ input) (coords.(configFind header footer) $ input) where
  header = "[begin lattice]"
  footer = "[end lattice]"

-- State array contains information on the state. Entity, occupation, dentacity
makeStateArray :: String -> StateArray
makeStateArray input = V.fromList $ (map V.fromList) $ stateList. (configFind header footer) $ input where
  header = "[begin lattice]"
  footer = "[end lattice]"


-- Adjacency list representation of the graph. Index is the vertex number
makeAdjList :: String -> AdjList
makeAdjList input = V.fromList $ neighbourList .(configFind header footer) $ input where
  header = "[begin lattice]"
  footer = "[end lattice]"



-- BEGIN Helper functions 
-- input list constructor for State Array
stateList :: [String] -> [[Int]]
stateList = map mapReadInt . map words .stateListChars where
  stateListChars = cutListBefore (=='"').(map $ drop 1).cutListAfter (=='|')
-- input list constructor for AdjList
neighbourList :: [String] -> [[Int]]
neighbourList = (map mapReadInt).(map $ wordsWhen comma).neighbourListChars where
  neighbourListChars = (cutListBefore (==']')).(map $ drop 1).(cutListAfter (=='['))

latvars :: [String] -> [String]
latvars = map (fst.break (==')'))
-- input list constructor for state array
siteTypes :: [String] -> [Int]
siteTypes = mapReadInt.siteTypesChars where
  siteTypesChars = (cutListBefore (==' ')).(cutListBefore (=='(')).latvars 

coords :: [String] -> [[Double]]
coords = (map mapReadDouble).(map $ wordsWhen comma).coordsChars where
  coordsChars = (map $ drop 1).(cutListAfter (=='(')).latvars

-- locates the config block and passes it to initialiser functions
configFind :: String -> String -> String -> [String]
configFind header footer = (takeWhile (/=footer)) . tail . (dropWhile (/=header)) . lines 

-- Trivial helper functions used to disect the input file

cutListBefore :: (a -> Bool) -> [[a]] -> [[a]]
cutListBefore p = map $ fst.break p
cutListAfter :: (a -> Bool) -> [[a]] -> [[a]]
cutListAfter p = map $ snd.break p

-- Read needs a type signature to work correctly. Neater than lambdas
mapReadInt :: [String] -> [Int]
mapReadInt = map read
mapReadDouble :: [String] -> [Double]
mapReadDouble = map read
-- Like Prelude.words but with a predicate
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'
comma :: Char -> Bool
comma = (==',')
