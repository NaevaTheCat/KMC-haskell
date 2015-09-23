module KMCtypes 
    where
--module containing types common to the simulation.
--and some basic operations on them.

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import qualified Data.List           as L
import qualified Data.Heap           as H

type AdjList    = V.Vector Neighbours
type StateArray = V.Vector State
type Neighbours = [Int]
type ReactionArray = V.Vector Reaction
type Mapping = [(Int,Int)]

data Process = Process
    { rTime :: Double
    , rIndex :: Int
    , mIndex :: Int
    , tAssign :: Double
    }deriving(Show)
from4Tuple (a,b,c,t) = Process a b c t
-- The process structure is for a heap. rTime gives priority
-- rIndex is i of mappedPoints rData while mIndex is j of the
-- same vector.

data ReactionData = ReactionData
    { reactions :: ReactionArray
    , mappedPoints :: V.Vector (V.Vector [(Int,Int)]) -- index i contains all the ways reaction i has been mapped to the lattice
    , inverse :: V.Vector (Species Int, Type Int, [Int]) --list of reactions involving sites with species and type whatever
    , sitesMapped :: V.Vector [Int] -- index i contains a list of the reactions that are currently counting on a sites state.
    , queue :: H.Heap Process
    , pRNs  :: [Double]
    }deriving(Show)
data Reaction = Reaction
    { iGraph :: AdjList
    , fGraph :: AdjList
    , iState :: StateArray
    , fState :: StateArray
    , patternLevel :: V.Vector Int -- pattern level wrt the ith thing in iState
    , rate   :: Double
    , newEnts :: [Int] -- labels of newentities between initial and final this is static
    }deriving(Show)

-- May want to add records e.g. Deposited
data Lattice = Lattice
    { lGraph :: AdjList
    , lState :: StateArray
    , tUpdated :: V.Vector (Double)
    , coords :: V.Vector Point
    } deriving(Show)
data State = State
    { siteType :: Type Int
    , entity :: Int
    , species :: Species Int
    , dentate :: Int
    } deriving(Show)
data Point = Point
    { xCord :: Float
    , yCord :: Float
    , cell  :: Int
    , sType :: Int
    }deriving(Show)

data Species a = Empty | Occupied a deriving(Show, Eq)
data Type a    = Wild  | Site a deriving(Show)

-- Permits wildcard sites
instance Eq a => Eq (Type a) where
    Wild == _ = True
    _ == Wild = True
    (Site a) == (Site b) = a == b
    Site a /= Site b = not (Site a == Site b)

-- Compares states ignorning entitiy. Necessary for mapping
instance Eq State where
    State t1 _ sp1 d1 == State t2 _ sp2 d2 =
        (t1 == t2) && (sp1 == sp2) && (d1 == d2)

instance Ord Process where
    compare a b = compare (rTime a) (rTime b)

instance Eq Process where
    a == b = (rTime a) == (rTime b)
    a /= b = not (a == b)
