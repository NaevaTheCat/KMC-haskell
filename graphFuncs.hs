--file KMC-haskell/graphFuncs.hs
--module for performing operations on graphs
module KMCgraph
    (
      Infinitable(..)
    , breadthSearch
    , permute
    ) where
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import GHC.Prim
import FIFO as F

-- Type synonmys, maybe newtype works better
type AdjList = V.Vector [Int]
type DistArr = V.Vector (Infinitable Int)
type ColourArr = V.Vector Colour
type VMatrix a = V.Vector (V.Vector a)

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
breadthSearch :: Monad m => AdjList -> Int -> Int -> m (V.Vector Int)
breadthSearch aL s td = do
    let cA = setColour (V.replicate (V.length aL) White) s Grey
    let dA = setDist (V.replicate (V.length aL) (Inf :: Infinitable Int)) s (Reg 0)
    let q  = F.enqueue s F.empty
    result <- whileBit aL q cA dA td
    return result

--COMPONENT
--While the queue has stuff in it this calls itself recursively
whileBit :: Monad m => AdjList -> FIFO Int -> ColourArr -> DistArr -> Int -> m (V.Vector Int)
whileBit aL q cA dA td = do
    let ((e:es),q') = F.remove 1 q
    case e of
         Just u -> if (checkDist dA u) < Reg td
                      then do
                          let nL = getN aL u
                          (cA',dA',q'') <- ifBit nL cA dA u q'
                          whileBit aL q'' cA' dA' td
                      else whileBit aL q' cA dA td
         Nothing -> return $ V.findIndices p cA where p x = (x == Grey) || (x == Black)

--COMPONENT
--Assuming we are interested in the Nth node this adds it to the search
ifBit :: Monad m => [Int] -> ColourArr -> DistArr -> Int -> FIFO Int 
    -> m (ColourArr, DistArr, FIFO Int)
ifBit [] cA dA u q = do
    let cA' = setColour cA u Black
    return (cA',dA,q)
ifBit nL@(n:nt) cA dA u q=
    case checkColour cA n of
         White -> do -- do not necessary
             let cA' = setColour cA n Grey
                 du  = checkDist dA u
                 dA' = setDist dA n (incInf du)
                 q'  = F.enqueue n q 
                 in ifBit nt cA' dA' u q'
         _     -> ifBit nt cA dA u q
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

permute m = goForth [] emptyq (-1) [] m where
    emptyq = V.replicate (V.length m) []
-- Well this is supposed to find the permutations of a matrix
goForth path queue depth successfulPaths m = 
    let newDepth  = depth+1
        areOnes   = V.toList $ V.findIndices (==1) $ m V.! newDepth
        newQueue  = writeQ queue newDepth areOnes 
        in case compare newDepth $ (V.length m) -1 of
                GT -> let successfulPaths' = path : successfulPaths in
                          goNext (tail path) queue depth successfulPaths' m
                _  -> goNext path newQueue newDepth successfulPaths m
decide path@(p:pt) queue depth sPs m = 
    case p `elem` pt of
         True  -> goNext pt queue depth sPs m
         False -> goForth path queue depth sPs m

goNext path queue depth sPs m =
    let (e,q') = deQ queue depth
        in case e of
                Just e' -> decide (e':path) q' depth sPs m
                Nothing -> goBack path q' depth sPs m

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

writeQ queue depth qs = V.modify (\v -> MV.write v depth qs) queue
