module KMCstates 
    ( entityLocations
    ) where
--basic module for manipulation of state arrays
--nonspecific to reaction or lattice
--
import KMCtypes
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Mutable as MV
import qualified Data.List as L
import GHC.Prim
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad

-- Given a state array this finds the invest mappings
-- for entity locations. That is, all the sites
-- occupied by a given entity
entityLocations :: StateArray -> [(Int, [Int])]
entityLocations sA = map sitesList . groupEntities . V.toList $
    runST (sortByEntities $ tupleWithSites sA)

--In place sorting of entities
sortByEntities ::V.Vector (Int,Int) ->  ST s (V.Vector (Int,Int))
sortByEntities v = do
    b <- V.thaw v
    _ <- VA.sortBy compareByFst b
    b1 <- V.freeze b
    return b1

compareByFst :: (Ord a) => (a,b1) -> (a,b2) -> Ordering
compareByFst a b = compare (fst a) (fst b)

groupEntities :: [(Int,Int)] -> [[(Int,Int)]]
groupEntities xs = L.groupBy (\a b -> (fst a) == (fst b)) xs

sitesList :: [(Int,Int)] -> (Int,[Int])
sitesList xs = (fst.head $ xs,foldr (\x acc -> (snd x):acc) [] xs)

tupleWithSites :: StateArray -> V.Vector (Int,Int)
tupleWithSites sA = V.imap (\i state -> (entity state, i)) sA

