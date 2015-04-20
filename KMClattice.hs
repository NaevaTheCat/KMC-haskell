module KMClattice where
-- Module for performing operations on the lattice
-- distinct functions from reaction although names are
-- shared
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import qualified Data.List           as L
import           KMCtypes

-- Get the ith state from a lattice
getState :: Lattice -> Int -> State
getState lattice i = (V.! i) . lState $ lattice
-- Get the Neighbours of the ith vertex in a lattice
getNeighbours :: Lattice -> Int -> Neighbours
getNeighbours lattice i = (V.! i) . lGraph $ lattice
-- fed a list of indexes retrieve the neighbours of these
getManyNeighbours :: Lattice -> [Int] -> [Neighbours]
getManyNeighbours lattice vs = map (getNeighbours lattice) vs

getManyState :: Lattice -> [Int] -> [State]
getManyState lattice ss = map (getState lattice) ss

-- can be used for one. Possibly better to use vector
-- state transform would be better
writeStates :: Lattice -> [(Int,State)] -> Lattice
writeStates lattice states = Lattice (lGraph lattice) newState where
                             newState   = statearray V.// states
                             statearray = lState lattice
                            
writeNeighbours :: Lattice -> [(Int,Neighbours)] -> Lattice
writeNeighbours lattice neighbours = Lattice newGraph (lState lattice) where
                                     newGraph = adjlist V.// neighbours
                                     adjlist  = lGraph lattice

-- Erases each vertex in the list of i by removing their neighbours
-- and trimming references to them from those neighbours
eraseVertices :: Lattice -> [Int] -> Lattice
eraseVertices lattice vs = 
    let n_of_vs = concat $ getManyNeighbours lattice vs 
        n_of_ns = getManyNeighbours lattice n_of_vs 
        -- Will break if duplicates end up in neighbours would prefer
        -- a fold or zip
        trimmed = map (L.\\ n_of_vs) n_of_ns
        newNeighbours = zip n_of_vs trimmed
        in writeNeighbours lattice $ (newNeighbours ++) . zip vs $ repeat []

