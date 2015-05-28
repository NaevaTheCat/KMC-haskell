import KMCtypes
import qualified Data.Vector as V
import qualified KMCreactions as R
import qualified Data.Heap as H

nList :: [[Int]]
nList = [[1],[0,2],[1,3],[2]]

emptyState :: Int -> State
emptyState e = State (Site 0) e (Empty) (1)

adjlistL = V.fromList nList
statearrL = V.fromList $ map emptyState [0..3]
neverUPL  = V.replicate (V.length adjlistL) 0.0

testLat :: Lattice
testLat = Lattice adjlistL statearrL neverUPL

testRdata = ReactionData rArry (V.replicate 2 V.empty) rInverse (V.replicate 4 []) testqueue prns

diffusion = Reaction (V.fromList [[1],[0]]) (V.fromList [[1],[0]]) rIstatesd rFstatesd (V.replicate 2 1) 1.0 []

deposition = Reaction (V.fromList [[]]) (V.fromList [[]]) (V.fromList [State Wild 0 Empty 1]) (V.fromList [State Wild 0 (Occupied 1) 1]) (V.replicate 1 0) 0.5 []

rIstatesd = V.fromList [rfullState 0, remptyState 1]
rFstatesd = V.fromList [remptyState 0, rfullState 1]
remptyState e = State Wild e Empty 1
rfullState e = State Wild e (Occupied 1) 1

rArry = V.fromList [diffusion, deposition]

rInverse = R.makeRInverse rArry

prns = R.newGen 1

testqueue = H.empty
