{-# LANGUAGE BangPatterns #-}

module Main where

import qualified KMC.Reactions as R
import KMC.Types
import qualified KMC.Lattice as KL
import qualified KMC.Config as C
import qualified Data.List as L
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import qualified Data.Vector as V
import qualified Data.Heap as H
import qualified Data.ByteString.Lazy as B
import System.Environment (getArgs)
import qualified System.Random as RNG
{-# LANGUAGE BangPatterns #-}
main :: IO ()
main = do
    [pA,xdim,ydim] <- getArgs
    seed <- RNG.randomIO
    let lattice = C.lattice (read xdim :: Int) (read xdim :: Int)
    let simTime = 0.0
    let counter = 0
    let pRNG =  R.newGen seed --temp seed for testing
    let rDATA = C.rData (read pA :: Double) (read xdim :: Int) (read ydim :: Int)
    let inverseR =  R.makeRInverse $ reactions rDATA
    let rData =  ReactionData (reactions rDATA) (mappedPoints rDATA) inverseR (sitesMapped rDATA) (queue rDATA) pRNG (tempMapStore rDATA)
    let rData' =  L.foldl' (\acc x -> R.tryReactions x lattice acc simTime) rData [0.. (V.length $ lGraph lattice) -1]
    recurseNext lattice counter rData' simTime 0.0 pA xdim ydim

recurseNext :: Lattice -> Int -> ReactionData -> Double -> Double -> String -> String -> String -> IO ()
recurseNext !lattice !counter !rData !simTime !simTimeOld !pA xdim ydim
    | peak == Nothing =  writeOut lattice rData simTime pA xdim ydim
    | simTime >= C.tEnd =  writeOut lattice rData simTime pA xdim ydim
    | (floor (simTime/C.tIncrement) - floor (simTimeOld/C.tIncrement)) >= 1 = do 
        writeOut lattice rData simTime pA xdim ydim
        let (l',c',rD',sT') = R.nextReaction rData lattice simTime counter -- will this be strict?
        recurseNext l' c' rD' sT' simTime pA xdim ydim -- simTime proxying for old
    | otherwise = 
        let (l',c',rD',sT') =  R.nextReaction rData lattice simTime counter -- will this be strict?
        in  recurseNext l' c' rD' sT' simTime pA xdim ydim -- simTime proxying for old
    where peak = H.viewMin $ queue rData

writeOut :: Lattice -> ReactionData -> Double -> String -> String -> String -> IO ()
writeOut l rD sT pA xdim ydim = 
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
        TIO.appendFile fileName $ T.pack $ "Pa :" ++ pA ++ "\n"
	TIO.appendFile fileName $ T.pack $ "xDim: " ++ xdim ++ "\n" ++ "yDim: " ++ ydim ++ "\n"
--        B.appendFile fileName ljson commented out for speed
--        B.appendFile fileName rDjson

pretty :: [Double] -> T.Text
pretty (e:co:o:[]) = T.pack $ "Empty: " ++ (show e) ++ "\n" ++
                     "CO: " ++ (show co) ++ "\n" ++
                     "O: " ++ (show o) ++ "\n"
speciesList = [Empty, Occupied 0, Occupied 1]
