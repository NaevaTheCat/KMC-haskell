module Main where

import qualified KMCreactions as R
import KMCtypes
import qualified KMClattice as KL
import qualified KMCconfig as C
import qualified Data.List as L
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import qualified Data.Vector as V
import qualified Data.Heap as H
import qualified Data.ByteString.Lazy as B
import Debug.Trace

main :: IO ()
main = do
    Prelude.putStrLn "foo"
    let lattice = trace "import lat" C.lattice
    let simTime = 0.0
    let counter = 0
    let pRNG = trace "making gen" $ R.newGen 0 --temp seed for testing
    let inverseR = trace "making inverse" $ R.makeRInverse $ reactions C.rData
    let rData = trace "making rdata" $ ReactionData (reactions C.rData) (mappedPoints C.rData) inverseR (sitesMapped C.rData) (queue C.rData) pRNG (tempMapStore C.rData)
    let rData' = trace "do the fold" $ L.foldl' (\acc x -> R.tryReactions x lattice acc simTime) rData [0.. (V.length $ lGraph lattice) -1]
    trace "start recursion" $ recurseNext lattice counter rData' simTime 0.0

recurseNext :: Lattice -> Int -> ReactionData -> Double -> Double -> IO ()
recurseNext lattice counter rData simTime simTimeOld 
    | peak == Nothing = putStrLn "aleph" >> writeOut lattice rData simTime
    | simTime >= C.tEnd = putStrLn "Beth" >> writeOut lattice rData simTime
    | (simTime - simTimeOld) > C.tIncrement = do 
        putStrLn "Daleth"
        writeOut lattice rData simTime
        let (l',c',rD',sT') = R.nextReaction rData lattice simTime counter -- will this be strict?
        recurseNext l' c' rD' sT' simTime -- simTime proxying for old
    | otherwise = 
        let (l',c',rD',sT') = trace "fuck?" $ R.nextReaction rData lattice simTime counter -- will this be strict?
        in putStrLn "He" >> recurseNext l' c' rD' sT' simTime -- simTime proxying for old
    where peak = H.viewMin $ queue rData

writeOut :: Lattice -> ReactionData -> Double -> IO ()
writeOut l rD sT = 
    let ljson = encode l
--        rDjson = encode rD
        spCounts = L.map (flip KL.percentSpecies l) speciesList

    in do
        time <- getCurrentTime
        tzone <- getTimeZone time
        let wallTimeString = show $ utcToLocalTime tzone time
        let simTimeString = show sT
        let fileName = "results/" ++ wallTimeString ++ "_" ++ simTimeString ++ ".txt"
        TIO.appendFile fileName $ pretty spCounts
        B.appendFile fileName ljson
--        B.appendFile fileName rDjson

pretty :: [Double] -> T.Text
pretty (e:co:o:[]) = T.pack $ "Empty: " ++ (show e) ++ "%" ++ "\n" ++
                     "CO: " ++ (show co) ++ "\n" ++
                     "O: " ++ (show o) ++ "\n"
speciesList = [Empty, Occupied 0, Occupied 1]
