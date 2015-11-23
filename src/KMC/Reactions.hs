--file KMC-haskell/KMCreactions.hs
--Module for performing operations involving
--reactions

module KMC.Reactions 
    ( nextReaction
    , makeRInverse
    , newGen
    , tryReactions
    ) where

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
import qualified Data.List           as L
import qualified KMC.Graph            as KG
import qualified KMC.Lattice          as KL
import KMC.Types
import qualified Data.Heap           as H
import qualified System.Random       as R
import qualified Data.HashMap.Strict        as Map
import qualified KMC.Config as C
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
    rMap = maybe [] id $ Map.lookup (Key $ rTime process) $ mappedPoints rData --need to make sure the empty list here results in everything doing nothing for a step
    ps = map fst rMap -- points that will change during R
    reaction =  (reactions rData) V.! (rIndex process)
    (newLat,newCounter) = performReaction counter reaction lattice rMap simTime'
    simTime' = (rTime process) 
    rData_u = L.foldl' (\acc x -> updateRData x lattice acc) rData ps
    rData_h  = ReactionData (reactions rData_u) (mappedPoints rData_u) (inverse rData_u) (sitesMapped rData_u) h' (pRNs rData_u) (tempMapStore rData_u)
    rData_f = L.foldl' (\acc x -> tryReactions x newLat acc simTime') rData_h ps
    in (newLat, newCounter,rData_f, simTime')

-- I think I wrote this to purge maps after a point was updated
-- good job past rachael. Future rachael hates you. What does this
-- actually do?
    -- Pretty sure I just need to delete the keys found there...
updateRData :: Int -> Lattice -> ReactionData 
    -> ReactionData
updateRData p lattice rData = 
    let dependantRKeys =  (sitesMapped rData) V.! p -- [Double] (keys)
        hashmap = mappedPoints rData
        newMList = L.foldl'
            (\acc x -> Map.delete x acc) hashmap dependantRKeys
        siteList'   = V.modify (\v -> MV.write v p []) (sitesMapped rData)
    in ReactionData (reactions rData) (newMList) (inverse rData) siteList' (queue rData) (pRNs rData) (tempMapStore rData)

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
           _    -> doMappings p lattice (eraseTempMaps rData) matchedRs simTime

-- formerly Full of stupid names and still untested
doMappings :: Int -> Lattice -> ReactionData -> V.Vector Int -> Double 
    -> ReactionData
doMappings p lattice rData matchedRs simTime = 
    let rindicies =  V.force (V.map (\x -> (inverse rData) V.! x) matchedRs)
        rsToMap =  V.fromList $ V.foldl' (\acc x -> acc ++ x) [] $ V.force (V.map (\(_,_,rs) -> map ((reactions rData) V.!) rs) rindicies) -- v [reaction]
        maps     = V.force (V.map (\x -> KG.goodMappings lattice x p) rsToMap)
        blarghly = V.fromList $ V.foldl' (\acc (_,_,x) -> acc ++ x) [] rindicies 
        rI_maps  = V.zip blarghly maps 
        hashmap  = mappedPoints rData
        mapStore = tempMapStore rData
        cleanrI  = checkDuplicates mapStore rI_maps -- V(i,V M)
        newMapStore  =  V.map (\(i,nM) -> (i,(mapStore V.! i) V.++ nM)) cleanrI
        finalMapStore     = V.update mapStore newMapStore
        sMped    = sitesMapped rData
        -- vector of list of lattice sites
        rI_sites = V.force (V.map (\(_,v) -> V.map (map fst) v) cleanrI)
        sMped'   = key_site_pair rI_sites keyVector sMped
        (heap,pRNG',keyVector) = enqueueR (V.map (\(rI,ms) -> (rI,V.length ms))cleanrI) rData simTime
        newQueue = H.union (queue rData) heap
        newHashMap = hashMapInsert keyVector (V.map snd cleanrI) hashmap
    in ReactionData (reactions rData) newHashMap (inverse rData) sMped' newQueue pRNG' finalMapStore

key_site_pair :: V.Vector (V.Vector [Int]) 
    -> V.Vector (V.Vector Key) 
    -> V.Vector [Key]
    -> V.Vector [Key]
key_site_pair vSite vKey sitesMap = 
    let site_key = V.map (uncurry key_site_helper) $ V.zip vSite vKey
    in  V.foldl' (\acc x -> V.accumulate (flip (:)) acc x) sitesMap site_key

key_site_helper :: V.Vector [Int] -> V.Vector Key -> V.Vector (Int,Key)
key_site_helper vI vK = 
    let vvI = V.map V.fromList vI
        v_ik =V.map (\(k,v) -> V.map (\i -> (i,k)) v) $ V.zip vK vvI
    in V.foldl' (\acc x -> acc V.++ x) V.empty v_ik

hashMapInsert :: V.Vector (V.Vector Key)
    -> V.Vector (V.Vector Mapping) -> Map.HashMap Key Mapping
    -> Map.HashMap Key Mapping
hashMapInsert kv mv hm = 
    let step1 = V.map (V.zipWith Map.insert) kv --converts kv to v(v a -> v f1)
        step2 = V.zipWith ($) step1 $ mv -- consumes mv and makes v(v f1)
        --ready for hashmap use folds to consume step2 pumping into the
        --initial hashmap v(v (M -> M))
    in V.foldl' innerfold hm step2 where
        innerfold = V.foldl' (\acc f -> f acc)
checkDuplicates :: V.Vector (V.Vector Mapping) -> V.Vector (Int,V.Vector Mapping)
    -> V.Vector (Int,V.Vector Mapping)
checkDuplicates tempStore v = let
    oldMaps i =  tempStore V.! i
    in V.force (V.map (\(i,x) -> 
        (i,(V.filter 
            (\y -> (== Nothing) $ V.find (listsID y) (oldMaps i)) x))) v)


listsID x y = L.null $ x L.\\ y    
-- For each reaction assigned a mapping generate a time
-- and enqueue it format is V (rI, nMaps)
enqueueR :: V.Vector (Int,Int) -> ReactionData -> Double
    -> (H.Heap Process,[Double],V.Vector (V.Vector Key))
enqueueR v rData simTime =
    let genL  = mapGen 0 (pRNs rData) rData simTime v
        heapL = map (H.fromList) $ map (\(x,_,_) -> x) genL 
        keyVector = V.fromList $ map V.fromList $ keyifier $  map (\(_,x,_) -> x) genL
    in (L.foldl' (\acc x -> H.union acc x) H.empty heapL , (\(_,_,x) -> x).last $ genL, keyVector)

keyifier :: [[Double]] -> [[Key]] -- change so not required. No point in 2 part keys
keyifier times = map (map (Key)) times
--Faster in reverse but slightly
mapGen :: Int -> [Double] -> ReactionData -> Double -> V.Vector (Int,Int)
--V.Vector (Int,[Int]) 
    -> [([Process],[Double],[Double])] -- middle are times
mapGen n pRNG rData simTime v
    | n == (V.length v) = []
    | otherwise = genResult : mapGen (n+1) ((\(_,_,x) -> x) genResult) rData simTime v
    where genResult =  genTimes pRNG (v V.! n) rData simTime

genTimes :: [Double] -> (Int,Int) -> ReactionData -> Double 
    -> ([Process],[Double],[Double]) -- middle is times, end is PRNS
genTimes pRNG (rI,ms) rData simTime = 
    let time x =  simTime - (log (1 - x)) / (rate . (V.! rI) . reactions $ rData)
        (rands,pRNG')  = genFloats pRNG ms
        procs  = map (\t -> (t,rI,simTime)) $ map time rands
    in  (map from4Tuple procs,map time rands, pRNG')
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

getStateI reaction i =  (V.! i) . iState $ reaction

getStateF reaction i =  (V.! i) . fState $ reaction

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

eraseTempMaps :: ReactionData -> ReactionData
eraseTempMaps rdata =
    let tms = tempMapStore rdata
        tms' = V.modify (\v -> MV.set v V.empty) tms
    in ReactionData (reactions rdata) (mappedPoints rdata) (inverse rdata) (sitesMapped rdata) (queue rdata) (pRNs rdata) tms'
