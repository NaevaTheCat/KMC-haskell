module KMCconfig where

import KMCtypes
import qualified Data.Vector as V
import qualified KMClattice as KL
import qualified Data.HashMap as Map
import qualified Data.Heap as H

xdim = 10
ydim = 10

tEnd = 1.0
tIncrement = 0.1

--pressureA = 0.55
emptyState e = State (Site 0) e Empty 0
occState e s = State Wild     e s     0
co = Occupied 0
o = Occupied 1
vac = Empty
wild = Wild 

twoLinkedPoints = V.fromList [[1],[0]]

lattice = Lattice
            (KL.simpleCubicGraph xdim ydim)
            (V.generate (xdim*ydim) emptyState)
            (V.replicate (xdim*ydim) 0.0)
            (KL.simpleCubic xdim ydim)

rData pA = ReactionData
            (V.fromList [o_ads pA, co_ads pA, o_co])
            (Map.empty)
            (V.empty)
            (V.replicate (xdim*ydim) [])
            (H.empty)
            ([])
            (V.replicate (xdim*ydim) V.empty)

o_ads pA = Reaction
            (twoLinkedPoints)
            (twoLinkedPoints)
            (V.fromList [occState 0 vac, occState 1 vac])
            (V.fromList [occState 0 o, occState 1 o])
            (V.replicate 2 1)
            (0.25*(1.0 - pA)*10)
            ([])
co_ads pA = Reaction
            (V.replicate 1 [])
            (V.replicate 1 [])
            (V.singleton (occState 0 vac))
            (V.singleton (occState 0 co))
            (V.singleton 0)
            (10 * pA)
            ([])

o_co = Reaction
            (twoLinkedPoints)
            (twoLinkedPoints)
            (V.fromList [occState 0 o, occState 1 co])
            (V.fromList [occState 0 vac, occState 1 vac])
            (V.replicate 2 1)
            (0.25 * 10.0^5)
            ([])
