--file KMC-haskell/KMCreactions.hs
--Module for performing operations involving
--reactions

module KMCreactions
    (

    ) where

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import qualified Data.List           as L
import qualified KMCgraph            as KG

type AdjList    = V.Vector [Int]
type StateArray = V.Vector (V.Vector Int)

data Reaction = Reaction
    { rGraph :: AdjList
    , rState :: StateArray}
