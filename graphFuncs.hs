--file KMC-haskell/graphFuncs.hs
--module for performing operations on graphs
module KMCgraph
    (
      Infinitable(..)
    , breadthSearch
    ) where
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import GHC.Prim
import FIFO as F
type AdjList = V.Vector [Int]
type DistArr = V.Vector (Infinitable Int)
type ColourArr = V.Vector Colour
data Colour = White | Grey | Black deriving(Eq,Show)
data Infinitable a = Inf | Reg a deriving(Eq, Show)
instance Ord a => Ord (Infinitable a) where
    compare Inf Inf = EQ
    compare Inf _ = GT
    compare (Reg a) (Reg b) = compare a b
-- increments Infinitable ints
incInf :: Infinitable Int -> Infinitable Int
incInf (Reg x) = Reg (x+1)
-- make the initial colours array for an Adj List
mkColourArr :: AdjList -> ColourArr
mkColourArr adjList = V.replicate (V.length adjList) White

mkDistArr :: AdjList -> DistArr
mkDistArr adjList = V.replicate (V.length adjList) (Inf :: Infinitable Int)
--Fetch neighbours
getN adjList i = adjList V.! i

checkColour :: ColourArr -> Int -> Colour
checkColour colourArr i = colourArr V.! i

setColour :: ColourArr -> Int -> Colour -> ColourArr
setColour colourArr i c= V.modify (\v -> MV.write v i c) colourArr 

checkDist :: DistArr -> Int -> Infinitable Int
checkDist distArr i = distArr V.! i

setDist :: DistArr -> Int -> Infinitable Int -> DistArr
setDist distArr i d = V.modify (\v -> MV.write v i d) distArr

--Breath first search until a target depth returning the indicies
--of the vertices within that depth form the source point
breadthSearch :: Monad m => AdjList -> Int -> Int -> m (V.Vector Int)
breadthSearch aL s td = do
    let cA = setColour (V.replicate (V.length aL) White) s Grey
    let dA = setDist (V.replicate (V.length aL) (Inf :: Infinitable Int)) s (Reg 0)
    let q  = F.enqueue s F.empty
    result <- whileBit aL q cA dA td
    return result
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
--Assuming we are interested in the Nth node this adds it to the search
ifBit :: Monad m => [Int] -> ColourArr -> DistArr -> Int -> FIFO Int 
    -> m (ColourArr, DistArr, FIFO Int)
ifBit [] cA dA u q = do
    let cA' = setColour cA u Black
    return (cA',dA,q)
ifBit nL@(n:nt) cA dA u q=
    case checkColour cA n of
         White -> do
             let cA' = setColour cA n Grey
             let du  = checkDist dA u
             let dA' = setDist dA n (incInf du)
             let q'  = F.enqueue n q
             ifBit nt cA' dA' u q'
         _     -> ifBit nt cA dA u q
