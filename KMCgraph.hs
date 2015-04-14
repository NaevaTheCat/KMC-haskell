--file KMC-haskell/graphFuncs.hs
--module for performing operations on graphs
module KMCgraph
    (
      Infinitable(..)
    , breadthSearch
    , permute
    , goodMappings
    ) where
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import qualified Data.List           as L
import GHC.Prim
import qualified FIFO                as F

-- Type synonmys, maybe newtype works better
type AdjList    = V.Vector [Int]
type StateArray = V.Vector (V.Vector Int)
type DistArr    = V.Vector (Infinitable Int)
type ColourArr  = V.Vector Colour
type VMatrix a  = V.Vector (V.Vector a)

-- data structures. Values for the search algorithm
data Colour = White | Grey | Black deriving(Eq,Show)
data Infinitable a = Inf | Reg a deriving(Eq, Show)
instance Ord a => Ord (Infinitable a) where
    compare Inf Inf = EQ
    compare Inf _ = GT
    compare (Reg a) (Reg b) = compare a b

--------------------
--Exported Functions
--------------------

--Breath first search until a target depth returning the indicies
--of the vertices within that depth form the source point
--MAIN
breadthSearch :: AdjList -> Int -> Int -> V.Vector Int
breadthSearch aL s td = 
    let cA = setColour (V.replicate (V.length aL) White) s Grey
        dA = setDist (V.replicate (V.length aL) Inf ) s (Reg 0)
        q  = F.enqueue s F.empty
    in whileBit aL q cA dA td

--COMPONENT
--While the queue has stuff in it this calls itself recursively
whileBit :: AdjList -> F.FIFO Int -> ColourArr -> DistArr -> Int -> V.Vector Int
whileBit aL q cA dA td = 
    let ((e:es),q') = F.remove 1 q
    in case e of
         Just u -> if (checkDist dA u) < Reg td
                      then 
                          let nL            = getN aL u
                              (cA',dA',q'') = ifBit nL cA dA u q'
                          in whileBit aL q'' cA' dA' td
                      else whileBit aL q' cA dA td
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
getN adjList i = adjList V.! i

getS stateArray i = stateArray V.! i

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
makeM :: StateArray -> StateArray -> AdjList -> AdjList -> VMatrix Int
makeM sA_RG sA_SG aL_RG aL_SG =
    onlyOnes statesCheck degreeCheck where
        statesCheck = tsdMat sA_RG sA_SG
        degreeCheck = degMat aL_RG aL_SG

extractSubgraph :: AdjList -> V.Vector Int -> AdjList
extractSubgraph graph vs = V.fromList $ cleanNeighbours vs $ getManyN graph vs

extractSubstate :: StateArray -> V.Vector Int -> StateArray
extractSubstate sA vs = V.fromList $ getManyS sA vs

getManyN adjList vs = map (getN adjList) $ V.toList vs

getManyS stateArr vs = map (getS stateArr) $ V.toList vs

mappings :: V.Vector Int -> [[Int]] -> [[(Int,Int)]]
mappings v rMs = map (zip vL) rMs where
    vL = V.toList v

cleanNeighbours :: V.Vector Int -> [[Int]] -> [[Int]]
cleanNeighbours vertices adjLists = map (L.intersect $ V.toList vertices) adjLists

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

degMat :: AdjList -> AdjList -> VMatrix Int
degMat aLrG aLsG = 
    V.generate rows (\i -> V.generate columns 
        (\j -> checkDegrees aLrG aLsG i j)) where
            rows    = V.length aLrG
            columns = V.length aLsG
-- need to modify for wildcard types
tsdCheck rG sG i j =
    let tsdSG = V.ifilter (\j _ -> j /= 1) sG
        tsdRG = V.ifilter (\i _ -> j /= 1) rG
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

--I'd like to neaten this to just taking a lattice and a reaction
goodMappings patternLevel largeGraph source statearray rGraph rState = 
    let targetD   = patternLevel
        vertices  = breadthSearch largeGraph source targetD
        subState  = extractSubstate statearray vertices
        subGraph  = extractSubgraph largeGraph vertices
        m         = makeM rState subState rGraph subGraph
        rawMaps   = permute m
    in mappings vertices rawMaps
