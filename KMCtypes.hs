module KMCtypes 
    where
--module containing types common to the simulation.
--and some basic operations on them.

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import qualified Data.List           as L

type AdjList    = V.Vector Neighbours
type StateArray = V.Vector State
type Neighbours = [Int]

data Reaction = Reaction
    { rGraph :: AdjList
    , iState :: StateArray
    , fState :: StateArray}

-- May want to add records e.g. Deposited
data Lattice = Lattice
    { lGraph :: AdjList
    , lState :: StateArray
    }
data State = State
    { siteType :: Type Int
    , entity :: Int
    , species :: Species Int
    , dentate :: Int
    } deriving(Show,Eq)

data Species a = Empty | Occupied a deriving(Show, Eq)
data Type a    = Wild  | Site a deriving(Show)

instance Eq a => Eq (Type a) where
    Wild == _ = True
    _ == Wild = True
    (Site a) == (Site b) = a == b
    Site a /= Site b = not (Site a == Site b)

