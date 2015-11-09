module Main where

import qualified KMCreactions as R
import KMCtypes
import qualified KMCconfig as C
import qualified Data.List as L

main :: IO ()
main = do
    lattice <- C.lattice
    rData <- C.rData
    simTime <- 0.0
    counter <- 0
    pRNG <- R.newGen 0 --temp seed for testing
    rData' <- L.foldl' (\acc x -> R.tryReactions x lattice acc simTime) rData [0..(-1).V.length (lGraph lattice)]

recurseNext :: Lattice -> Int -> ReactionData -> Double -> Double -> ??
recurseNext lattice counter rData simTime simTimeOld 
    | peak == Nothing = writeOut
    | simTime >= tEnd = writeOut
    | (simTime - simTimeOld) > tIncrement = do writeOut
                                               R.nextReaction rData lattice simTime counter -- will this be strict?
    | otherwise = 
        let (l',c',rD',sT') = R.nextReaction rData lattice simTime counter -- will this be strict?
        in recurseNext l' c' rD' sT' simTime -- simTime proxying for old
    where peak = H.viewMin $ queue rData


