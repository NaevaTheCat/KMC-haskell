--file KMC-haskell/KMCreactions.hs
--Module for performing operations involving
--reactions

module KMCreactions
     

      where

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import qualified Data.List           as L
import qualified KMCgraph            as KG
import qualified KMClattice          as KL
import KMCtypes
import qualified Data.Heap           as H
import qualified System.Random       as R

nextReaction :: ReactionData -> Lattice -> Double -> Int 
    -> (Lattice, Int, ReactionData, Double)
nextReaction rData lattice simTime counter = let
    peak = H.viewMin (queue rData)
    in case peak of
         Just (process,h') -> proceed rData lattice simTime counter process h'
         Nothing -> error "Change this to halting. There are no reactions"

proceed :: ReactionData -> Lattice -> Double -> Int -> Process -> H.Heap Process 
    -> (Lattice, Int, ReactionData, Double)
proceed rData lattice simTime counter process h' = let
    rMap = (V.! (mIndex process)).(V.! (rIndex process)) $ mappedPoints rData
    ps = map fst rMap -- points that will change during R
    reaction = (reactions rData) V.! (rIndex process)
    (newLat,newCounter) = performReaction counter reaction lattice rMap simTime'
    simTime' = (rTime process) + simTime
    rData_u = L.foldl' (\acc x -> updateRData x lattice acc) rData ps
    rData_h  = ReactionData (reactions rData_u) (mappedPoints rData_u) (inverse rData_u) (sitesMapped rData_u) h' (pRNs rData_u)
    rData_f = L.foldl' (\acc x -> tryReactions x newLat acc simTime') rData_h ps
    in (newLat, newCounter,rData_f, simTime')

-- I think I wrote this to purge maps after a point was updated
updateRData :: Int -> Lattice -> ReactionData 
    -> ReactionData
updateRData p lattice rData = 
    let dependantRs = (sitesMapped rData) V.! p
        mappingList = mappedPoints rData
        vMaps       = map (mappingList V.!) dependantRs
        newVmaps    = zip dependantRs $ map (exclude p) vMaps
        newMList    = mappingList V.// newVmaps
        siteList'   = V.modify (\v -> MV.write v p []) (sitesMapped rData)
    in ReactionData (reactions rData) (newMList) (inverse rData) siteList' (queue rData) (pRNs rData)

exclude :: Int -> V.Vector Mapping 
    -> V.Vector Mapping
exclude p vMaps = V.filter (not.elem p. map fst) vMaps
-- a point P has just changed. Find what reactions could maybe happen
-- given the characteristics of P. Take the successful mappings
-- and add them to the end of the appropriate record.
-- For each of the lattice points involved in the mapping, record that
-- this reaction is depending on those points.
tryReactions :: Int -> Lattice -> ReactionData -> Double 
    -> ReactionData
tryReactions p lattice rData simTime = 
    let pState = KL.getState lattice p
        (pSpec,pType) = (species pState, siteType pState)
        --can this be empty?
        matchedRs = V.findIndices 
            (\(a,b,_) -> (a==pSpec) && (b == pType)) $ inverse rData
    in case V.null matchedRs of
           True -> rData
           _    -> doMappings p lattice rData matchedRs simTime

-- Full of stupid names and untested
doMappings :: Int -> Lattice -> ReactionData -> V.Vector Int -> Double 
    -> ReactionData
doMappings p lattice rData matchedRs simTime = 
    let rsToMap  = V.map (\x -> (reactions rData) V.! x) matchedRs
        maps     = V.map (\x -> KG.goodMappings lattice x p) rsToMap
        rI_maps  = V.zip matchedRs maps --second thing is a vector of mappings
        mappedPs = mappedPoints rData
        cleanrI  = checkDuplicates rData rI_maps
        newMaps  = V.map (\(i,nM) -> (i,(mappedPs V.! i) V.++ nM)) cleanrI
        mPs'     = V.update mappedPs newMaps
        sMped    = sitesMapped rData
        -- vector of list of lattice sites
        rI_sites = V.map (\(a,b) -> 
            (a, L.nub .map fst . concat . V.toList $ b)) rI_maps
        newSites = V.concatMap (\(i,sites) -> 
            V.fromList $ zip sites 
                (map (L.nub . (i:) . (sMped V.!)) sites)) rI_sites
        sMped'   = V.update sMped newSites
        (heap,pRNG') = enqueueR cleanrI rData simTime
        newQueue = H.union (queue rData) heap
    in ReactionData (reactions rData) mPs' (inverse rData) sMped' newQueue pRNG'

checkDuplicates :: ReactionData -> V.Vector (Int,V.Vector Mapping)
    -> V.Vector (Int,V.Vector Mapping)
checkDuplicates rData v = let
    oldMaps i = (mappedPoints rData) V.! i
    in V.map (\(i,x) -> 
        (i,(V.filter 
            (\y -> (== Nothing) $ V.find (listsID y) (oldMaps i)) x))) v


listsID x y = L.null $ x L.\\ y    
-- For each reaction assigned a mapping generate a time
-- and enqueue it format is V (rI, v maps)
enqueueR :: V.Vector (Int, V.Vector Mapping) -> ReactionData -> Double
    -> (H.Heap Process,[Double])
enqueueR v rData simTime =
    let mIndicies = V.map (\(a,b) -> (a, map 
            ((+) . V.length . (V.! a) . mappedPoints $ rData) 
                [0.. (V.length b - 1)])) v
        genL  = mapGen 0 (pRNs rData) rData simTime mIndicies
        heapL = map (H.fromList) $ map fst genL 
    in (L.foldl' (\acc x -> H.union acc x) H.empty heapL , snd.last $ genL)

--Faster in reverse but slightly
mapGen :: Int -> [Double] -> ReactionData -> Double -> V.Vector (Int,[Int]) 
    -> [([Process],[Double])]
mapGen n pRNG rData simTime v
    | n == (V.length v) = []
    | otherwise = genResult : mapGen (n+1) (snd genResult) rData simTime v
    where genResult = genTimes pRNG (v V.! n) rData simTime

genTimes :: [Double] -> (Int,[Int]) -> ReactionData -> Double 
    -> ([Process],[Double])
genTimes pRNG (rI,ms) rData simTime = 
    let time x = simTime - (log (1 - x)) / (rate . (V.! rI) . reactions $ rData)
        (rands,pRNG')  = genFloats pRNG $ length ms
        procs  = map (\(t,m) -> (t,rI,m,simTime)) $ zip (map time rands) ms
    in  (map from4Tuple procs, pRNG')
-- Given a reaction has been selected update the relevant
-- sites on states on the lattice with the final configuration.
-- if a new entity is created and cannot be assigned the same
-- number as an old one then the next available number must be
-- used
-- have [(iLat,iReac)] need [(iLat, State Rf)] where entities
-- are changed also type needs to be lattice type
performReaction :: Int -> Reaction -> Lattice -> Mapping -> Double
    -> (Lattice, Int)
performReaction counter reaction lattice mapping simTime =
    let (eMap, c') = entityMap counter reaction lattice mapping
        latStates = KL.getManyState lattice (map fst mapping)
        reacStates = V.toList $ fState reaction
        newStates = map 
            (\(a,b) -> stateUpdate eMap a b) (zip latStates reacStates)
    in (KL.writeStates lattice (zip (map fst mapping) newStates) simTime, c')

stateUpdate eMap sL sR = 
    State (siteType sL) (latE) (species sR) (dentate sR) where
        latE = case (lookup (entity sR) eMap) of
                Just x -> x
                _      -> error "entity couldn't be found"

entityMap :: Int -> Reaction -> Lattice -> [(Int,Int)] 
    -> ([(Int,Int)],Int)
entityMap counter reaction lattice lrMap 
    | null newEs = (zip eR eL , counter)
    | otherwise  = ((zip eR eL) ++ rest, lastCount) where
        newEs = newEnts reaction
        (rest,lastCount) = stapleNew newEs counter
        eL = map (entity . KL.getState lattice . fst) lrMap
        eR = map (entity . getStateI  reaction . snd) lrMap

getStateI reaction i = (V.! i) . iState $ reaction

getStateF reaction i = (V.! i) . fState $ reaction

--Expensive operation compares the entities in a reaction initially
--and finally to locate a difference. Any new entities need to be
--assigned new labels.
newEntities :: Reaction -> [Int]
newEntities reaction = (L.nub esF) L.\\ (L.nub esI) where
    esI = map (entity . getStateI reaction) [0..limit]
    esF = map (entity . getStateF reaction) [0..limit]
    limit = V.length $ iState reaction

-- This is hideous. There has to be a better way to pass a bit
-- of state around and get the new state out at the end
stapleNew :: [Int] -> Int -> ([(Int,Int)],Int)
stapleNew newEs lastID = (zip newEs [(lastID+1)..], finalID) where
    finalID = lastID + (length newEs)

makeRInverse :: ReactionArray -> V.Vector (Species Int, Type Int, [Int])
makeRInverse rArr = V.fromList result where
    iAndReqs = V.toList $ V.imap (\i x -> (i,requisets x)) rArr
    allReqs  = L.nub .concat .  map snd $ iAndReqs
    result = map (\x@(a,b) -> (a,b, map fst . filter (\y -> elem x (snd y)) $ iAndReqs)) allReqs

requisets :: Reaction -> [(Species Int, Type Int)]
requisets reaction = 
    V.toList $ V.map (\x -> (species x, siteType x)) (iState reaction)


genFloats :: [Double] -> Int -> ([Double],[Double])
genFloats l n = (take n l, drop n l)

newGen :: Int -> [Double]
newGen seed = R.randoms $ R.mkStdGen seed
