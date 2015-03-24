--file KMC/importfuncs.hs
import qualified Data.Vector as V

makeLattice :: String -> V.Vector (Int,[Double])
makeLattice input = V.fromList $ zip (siteTypes.configFind $ input) (coords.configFind $ input)

latvars :: [String] -> [String]
latvars = map (fst.break (==')'))

siteTypes :: [String] -> [Int]
siteTypes = mapReadInt.siteTypesChars where
  siteTypesChars = (cutListB (==' ')).(cutListB (=='(')).latvars 

coords :: [String] -> [[Double]]
coords = (map mapReadDouble).(map $ wordsWhen comma).coordsChars where
  coordsChars = (map $ drop 1).(cutListA (=='(')).latvars

-- locates the config block and passes it to initialiser functions
configFind :: String -> [String]
configFind = (takeWhile (/=footer)) . tail . (dropWhile (/=header)) . lines where
  header = "[begin config]"
  footer = "[end config]"

-- Trivial helper functions used to disect the input file

cutListB :: (a -> Bool) -> [[a]] -> [[a]]
cutListB p = map $ fst.break p
cutListA :: (a -> Bool) -> [[a]] -> [[a]]
cutListA p = map $ snd.break p

mapReadInt :: [String] -> [Int]
mapReadInt = map read
mapReadDouble :: [String] -> [Double]
mapReadDouble = map read

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'
comma :: Char -> Bool
comma = (==',')
