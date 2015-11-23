module KMC.Lattice where
-- Module for performing operations on the lattice
-- distinct functions from reaction although names are
-- shared
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import qualified Data.List           as L
import           KMC.Types
import Debug.Trace
-- Get the ith state from a lattice
getState :: Lattice -> Int -> State
getState lattice i =  (V.! i) . lState $ lattice
-- Get the Neighbours of the ith vertex in a lattice
getNeighbours :: Lattice -> Int -> Neighbours
getNeighbours lattice i =  (V.! i) . lGraph $ lattice
-- fed a list of indexes retrieve the neighbours of these
getManyNeighbours :: Lattice -> [Int] -> [Neighbours]
getManyNeighbours lattice vs = map (getNeighbours lattice) vs

getManyState :: Lattice -> [Int] -> [State]
getManyState lattice ss = map (getState lattice) ss

percentSpecies :: Species Int -> Lattice -> Double
percentSpecies s l = (count / total) * 100.0 where
    count = fromIntegral $ V.foldl' (speciesCount s) 0 $ lState l
    total = fromIntegral $ V.length $ lState l

speciesCount :: Species Int -> Int -> State -> Int
speciesCount orig c comp
    | (species comp) == orig = c+1
    | otherwise = c
-- can be used for one. Possibly better to use vector
-- state transform would be better
-- probably want in place updating
writeStates :: Lattice -> [(Int,State)] -> Double -> Lattice
writeStates lattice states simTime = Lattice (lGraph lattice) newState newTime (coords lattice)
    where newState   = statearray V.// states
          statearray = lState lattice
          newTime = (tUpdated lattice) V.// (map (\(i,_) -> (i,simTime)) states)

writeNeighbours :: Lattice -> [(Int,Neighbours)] -> Lattice
writeNeighbours lattice neighbours = Lattice newGraph (lState lattice) t (coords lattice) where
                                     newGraph = adjlist V.// neighbours
                                     adjlist  = lGraph lattice
                                     t = tUpdated lattice

-- Erases each vertex in the list of i by removing their neighbours
-- and trimming references to them from those neighbours
-- keeps their index and state. maybe good? means searches become
-- more intensive with time. Probably want to garbage collect every now
-- and then
eraseVertices :: Lattice -> [Int] -> Lattice
eraseVertices lattice vs = 
    let n_of_vs = concat $ getManyNeighbours lattice vs 
        n_of_ns = getManyNeighbours lattice n_of_vs 
        -- Will break if duplicates end up in neighbours would prefer
        -- a fold or zip
        trimmed = map (L.\\ n_of_vs) n_of_ns
        newNeighbours = zip n_of_vs trimmed
        in writeNeighbours lattice $ (newNeighbours ++) . zip vs $ repeat []

-- Working here, initialisation
addToX n point = Point ((+ n) . xCord $ point) (yCord point) (cell point) (sType point)

addToY n point = Point  (xCord point) ((+ n) . yCord $ point)(cell point) (sType point)

simpleCubic :: Int -> Int -> V.Vector Point
simpleCubic x y =
    let p = Point 0.0 0.0 0 0
        xVec = V.generate x (\i -> addToX ((fromIntegral i) * 1.0) p)
        xyVec = V.generate y (\i -> V.map (addToY ((fromIntegral i)*1.0)) xVec)
    in  V.concat $ map (xyVec V.!) [0..V.length xyVec - 1]

simpleCubicGraph :: Int -> Int -> AdjList
simpleCubicGraph x y = V.fromList $ map (simpleCubeNeigh x y) [0..x*y -1]

simpleCubeNeigh x y i = [nx1,nx2,ny1,ny2] where
    position = (mod i x, div i x) -- col, row
    nx1 = case fst position == 0 of
               True -> i + (x - 1)
               False -> i - 1
    nx2 = case fst position == x-1 of
              True -> i - (x - 1)
              False -> i + 1
    ny1 = case snd position == 0 of
               True -> (x*y) - (x - i)
               False -> i - x
    ny2 = case snd position == y-1 of
               True -> fst position
               False -> i + x
