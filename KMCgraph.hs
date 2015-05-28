--file KMC-haskell/graphFuncs.hs
--module for performing operations on graphs
module KMCgraph
--    (
--      Infinitable(..)
--    , breadthSearch
--    , permute
--    , goodMappings
--    , confirmMappings
--    ) 
      where
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import qualified Data.List           as L
import           GHC.Prim
import qualified FIFO                as F
import           KMCtypes
import qualified KMClattice          as KL
import qualified KMCstates           as KS

-- Type synonmys, maybe newtype works better
type DistArr    = V.Vector (Infinitable Int)
type ColourArr  = V.Vector Colour
type VMatrix a  = V.Vector (V.Vector a)

-- data structures. Values for the search algorithm
data Colour = White | Grey | Black deriving(Eq,Show)

-- Infinite value for distances not calculated. Included
-- so searches under a distance d do not return unsearched
-- point. Avoids doing silly things with negative numbers
data Infinitable a = Inf | Reg a deriving(Eq, Show)
instance Ord a => Ord (Infinitable a) where
    compare Inf Inf = EQ
    compare Inf _   = GT
    compare _   Inf = LT
    compare (Reg a) (Reg b) = compare a b

--------------------
--Exported Functions
--------------------

--Breath first search until a target depth returning the indicies
--of the vertices within that depth form the source point
--MAIN
breadthSearch :: Lattice -> Int -> Int -> V.Vector Int
breadthSearch lattice s td = 
    let aL = lGraph lattice
        cA = setColour (V.replicate (V.length aL) White) s Grey
        dA = setDist (V.replicate (V.length aL) Inf ) s (Reg 0)
        q  = F.enqueue s F.empty
    in whileBit lattice q cA dA td

--COMPONENT
--While the queue has stuff in it this calls itself recursively
whileBit :: Lattice -> F.FIFO Int -> ColourArr -> DistArr -> Int -> V.Vector Int
whileBit lattice q cA dA td = 
    let ((e:es),q') = F.remove 1 q
    in case e of
         Just u -> if (checkDist dA u) < Reg td
                      then 
                          let nL            = KL.getNeighbours lattice u
                              (cA',dA',q'') = ifBit nL cA dA u q'
                          in whileBit lattice q'' cA' dA' td
                      else whileBit lattice q' cA dA td
         Nothing -> V.findIndices p cA 
            where p x = (x /= White)

--COMPONENT
--Assuming we are interested in the Nth node this adds it to the search
ifBit :: [Int] -> ColourArr -> DistArr -> Int -> F.FIFO Int 
    -> (ColourArr, DistArr, F.FIFO Int)
ifBit [] cA dA u q = 
    let cA' = setColour cA u Black
    in (cA',dA,q)
ifBit nL@(n:nt) cA dA u q=
    case checkColour cA n of
        White -> 
            let cA' = setColour cA n Grey
                du  = checkDist dA u
                dA' = setDist dA n (incInf du)
                q'  = F.enqueue n q 
            in ifBit nt cA' dA' u q'
        _     -> ifBit nt cA dA u q

--MAIN
--Find all the permutation matricies of a given matrix m
--Used for subgraph isomorphisms in Ullmann's algorithm
--Returns a list of paths where the nth entry in a path is
--the index of the entry in the subgraph that maps to
--the nth member of the reaction graph
permute :: VMatrix Int -> [[Int]]
permute m = map reverse $ goForth [] emptyq (-1) [] m where
    emptyq = V.replicate (V.length m) []

--COMPONENT
--Proceed a depth enqueing possibilities at that depth for testing
--or accept a complete path and test the next terminating sequence.
goForth :: [Int] -> V.Vector [Int] -> Int -> [[Int]] -> VMatrix Int -> [[Int]]
goForth path queue depth successfulPaths m = 
    let newDepth  = depth+1
        areOnes   = V.toList $ V.findIndices (==1) $ m V.! newDepth
        newQueue  = writeQ queue newDepth areOnes 
    in case compare newDepth $ (V.length m) -1 of
            GT -> let successfulPaths' = path : successfulPaths 
                  in  goNext (tail path) queue depth successfulPaths' m
            _  -> goNext path newQueue newDepth successfulPaths m

--Is the index being considered unique to the sequence
decide path@(p:pt) queue depth sPs m = 
    case p `elem` pt of
         True  -> goNext pt queue depth sPs m
         False -> goForth path queue depth sPs m

--Grab the next element for examination form the queue or
--goBack if this level is tested completely for the current path
goNext path queue depth sPs m =
    let (e,q') = deQ queue depth
        in case e of
                Just e' -> decide (e':path) q' depth sPs m
                Nothing -> goBack path q' depth sPs m

--Return one depth and consider the relevant element OR terminate
--returning the paths that were completed
goBack path queue depth sPs m =
    let newDepth = depth -1
        in case newDepth of
             -1 -> sPs
             _  -> goNext (tail path) queue newDepth sPs m

deQ queue depth = (e,q) where
    (e,es)           = takeFrom (queue V.! depth)
    q                = writeQ queue depth es
    takeFrom []      = (Nothing, [])
    takeFrom (x:xs)  = (Just x, xs)

------------------
--Helper Functions
------------------
-- increments Infinitable ints
incInf :: Infinitable Int -> Infinitable Int
incInf (Reg x) = Reg (x+1)

-- make the initial colours array for an Adj List
mkColourArr :: AdjList -> ColourArr
mkColourArr adjList = V.replicate (V.length adjList) White

mkDistArr :: AdjList -> DistArr
mkDistArr adjList = V.replicate (V.length adjList) (Inf :: Infinitable Int)
--Fetch neighbours of the ith entry

checkColour :: ColourArr -> Int -> Colour
checkColour colourArr i = colourArr V.! i

setColour :: ColourArr -> Int -> Colour -> ColourArr
setColour colourArr i c= V.modify (\v -> MV.write v i c) colourArr 

checkDist :: DistArr -> Int -> Infinitable Int
checkDist distArr i = distArr V.! i

setDist :: DistArr -> Int -> Infinitable Int -> DistArr
setDist distArr i d = V.modify (\v -> MV.write v i d) distArr


-- VMatrix helpers
mapOnAll :: (a -> b) -> VMatrix a -> VMatrix b
mapOnAll f matrix = V.map (V.map f) matrix

-- destructive update on row i with f
mapOnRow :: Int -> (a -> a) -> VMatrix a -> VMatrix a
mapOnRow i f matrix = V.modify ((\sv v -> MV.write v i sv) $ V.map f $ matrix V.! i) matrix 

getColumn j matrix = V.map (V.! j) matrix

-- In place update of a Vector (FILO) at queue corresponding to depth
writeQ queue depth qs = V.modify (\v -> MV.write v depth qs) queue

-- WORKING
makeM :: Reaction -> Lattice -> VMatrix Int
makeM reaction lattice = 
    onlyOnes statesCheck degreeCheck where
        statesCheck = tsdMat (iState reaction) (lState lattice)
        degreeCheck = degMat (iGraph reaction) (lGraph lattice)

extractSubLattice :: Lattice -> V.Vector Int -> Lattice
extractSubLattice lat vs = Lattice subgraph subState dummyTimes where
    vL = V.toList vs
    subgraph = V.fromList $ cleanNeighbours vL $ KL.getManyNeighbours lat vL
    subState = V.fromList $ KL.getManyState lat vL
    dummyTimes = V.replicate (V.length subgraph) 0.0
    --WARNING CHANGED 15-05-25 with types update

-- This is effectively a lookup table of(iLarge,iReaction) 
-- it's actually a list of lookup tables for the possible
-- reactions.
mappings :: V.Vector Int -> [[Int]] -> V.Vector [(Int,Int)]
mappings v rMs = V.fromList $ map (flip zip [0..]) iLarge where
    iLarge = map (map (v V.!)) rMs 

cleanNeighbours :: [Int] -> [Neighbours] -> [Neighbours]
cleanNeighbours vertices adjLists = map (L.intersect vertices) adjLists

-- Crudely just making N matricies of 1 or 0 for passing/failing
-- the test. Then multiply elementwise to get the final m. Can be made
-- more efficient by carrying out expensive tests only on relevant entries
-- last

checkDegrees rG sG i j = 
    let degSG = length $ sG V.! j
        degRG = length $ rG V.! i
    in case compare degSG degRG of
            LT -> 0
            _  -> 1
-- Rows are entries in the reaction, columns are entries in the subgraph
degMat :: AdjList -> AdjList -> VMatrix Int
degMat aLrG aLsG = 
    V.generate rows (\i -> V.generate columns 
        (\j -> checkDegrees aLrG aLsG i j)) where
            rows    = V.length aLrG
            columns = V.length aLsG

tsdCheck rG sG i j =
    let tsdSG = sG V.! j
        tsdRG = rG V.! i
    in case tsdSG == tsdRG of
            True    -> 1
            False   -> 0
  
tsdMat :: StateArray -> StateArray -> VMatrix Int
tsdMat sArG sAsG = 
    V.generate rows (\i -> V.generate columns 
        (\j -> tsdCheck sArG sAsG i j)) where
            rows    = V.length sArG
            columns = V.length sAsG

onlyOnes :: VMatrix Int -> VMatrix Int -> VMatrix Int
onlyOnes m1 m2 =  zipVMwith (*) m1 m2

zipVMwith :: (a -> b -> c) -> (VMatrix a -> VMatrix b -> VMatrix c)
zipVMwith f = V.zipWith (V.zipWith f)


goodMappings :: Lattice -> Reaction -> Int -> V.Vector [(Int,Int)]
goodMappings lattice reaction source = 
    let targetD       = (patternLevel reaction) V.! matchIndex
        stateSource   = KL.getState lattice source
        (Just matchIndex) = V.findIndex (== stateSource) (iState reaction)
        vertices      = breadthSearch lattice source targetD
        subLattice    = extractSubLattice lattice vertices
        m             = makeM reaction subLattice
        rawMaps       = permute m
        latSites      = map snd $ KS.entityLocations (lState lattice)
        reacSites     = map snd $ KS.entityLocations (iState reaction)
        validMaps     = confirmMappings latSites reacSites rawMaps
    in mappings vertices validMaps

       
rToS :: [Int] -> [(Int,Int)] -> [Maybe Int]
rToS r_sites lUL = map (flip lookup lUL) r_sites

cleanInvalidMaps :: [Maybe Int] -> [Int]
cleanInvalidMaps rs 
    | (Nothing `elem` rs) == True = []
    | otherwise = map (\(Just x) -> x) rs

confirmMappings :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]]
confirmMappings _ _ [] = []
confirmMappings latSites reacSites (rM:rMs) =
    let lUL = zip [0..] rM
        rInTermsOfS = map (cleanInvalidMaps . flip rToS lUL) reacSites
-- This mess is for reducing checking all the mappings to a single T/F
        lowFold l = L.foldl' -- changed high to and I think that's right
            (\acc a -> (|| acc).(== l).(L.intersect l) $ a) False
        highFold list = L.foldl' 
            (\acc x -> (&& acc).(flip lowFold list) $ x) True
    in case (highFold latSites rInTermsOfS) of
            True -> rM : confirmMappings latSites reacSites rMs
            False -> confirmMappings latSites reacSites rMs
