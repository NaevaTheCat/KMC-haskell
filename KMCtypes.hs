{-# LANGUAGE OverloadedStrings #-}
module KMCtypes 
    where
--module containing types common to the simulation.
--and some basic operations on them.
import Data.Text
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import qualified Data.List           as L
import qualified Data.Heap           as H
import qualified Data.HashMap.Strict        as Map
import Data.Hashable
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative

type AdjList    = V.Vector Neighbours
type StateArray = V.Vector State
type Neighbours = [Int]
type ReactionArray = V.Vector Reaction
type Mapping = [(Int,Int)]

data Process = Process
    { rTime :: Double
    , rIndex :: Int
    , tAssign :: Double
    }deriving(Show)
instance ToJSON Process where
    toJSON (Process rT rI tA) = object ["rTime" .= rT, "rIndex" .= rI, "tAssign" .= tA]

newtype Key = Key {keyValue :: Double}
    deriving(Eq, Ord, Show)

instance FromJSON Key where
    parseJSON k = Key <$> parseJSON k

instance ToJSON Key where
    toJSON (Key k) = toJSON k

instance Hashable Key where
    hashWithSalt x (Key d) = hashWithSalt x d

from4Tuple (a,b,c) = Process a b c 

data ReactionData = ReactionData
    { reactions :: ReactionArray
    , mappedPoints :: Map.HashMap Key [(Int,Int)]
    --V.Vector (V.Vector [(Int,Int)]) -- index i contains all the ways reaction i has been mapped to the lattice
    , inverse :: V.Vector (Species Int, Type Int, [Int]) --list of reactions involving sites with species and type whatever
    , sitesMapped :: V.Vector [Key] -- i is keys of reactions depending on point i
    , queue :: H.Heap Process
    , pRNs  :: [Double]
    , tempMapStore :: V.Vector (V.Vector Mapping)
    }deriving(Show)
--instance ToJSON ReactionData where
--    toJSON (ReactionData _ mP _ sM q _ _) = object ["maps" .= mP, "sitesMapped" .= sM, "queue" .= q]
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
instance ToJSON Lattice where
    toJSON (Lattice lg ls tu co) = object ["Graph" .= lg, "State" .= ls, "Updated" .= tu, "Coords" .= co]
data State = State
    { siteType :: Type Int
    , entity :: Int
    , species :: Species Int
    , dentate :: Int
    } deriving(Show)
instance ToJSON State where
    toJSON (State sT e sP d) = object ["siteType" .= sT, "entity" .= e, "species" .= sP, "dentate" .= d]
data Point = Point
    { xCord :: Float
    , yCord :: Float
    , cell  :: Int
    , sType :: Int
    }deriving(Show)

instance ToJSON Point where
    toJSON (Point x y c sT) = object ["x" .= x, "y" .= y, "cell" .= c, "siteType" .= sT]

data Species a = Empty | Occupied a deriving(Show, Eq)

instance (FromJSON a) => FromJSON (Species a) where
    parseJSON Null = pure Empty 
    parseJSON a = Occupied <$> parseJSON a
    parseJSON _ = fail "wrongType"

instance (ToJSON a) => ToJSON (Species a) where
    toJSON Empty = Null
    toJSON (Occupied a) = toJSON a

data Type a = Wild  | Site a deriving(Show)

instance (FromJSON a) => FromJSON (Type a) where
    parseJSON Null = pure Wild
    parseJSON a = Site <$> parseJSON a

instance (ToJSON a) => ToJSON (Type a) where
    toJSON Wild = Null
    toJSON (Site a) = toJSON a
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

