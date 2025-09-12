{-# LANGUAGE MultiWayIf #-}

module BinarySearch.BinarySearch (search, searchRec)
where

import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Vector (Vector)
import qualified Data.Vector as V

searchRec :: Vector Int -> Int -> Int
searchRec nums target = loop 0 (V.length nums - 1)
  where
    loop start end
        | start > end = -1
        | otherwise = do
            let mid = (start + end) `div` 2
            let val = nums V.! mid
            if
                    | V.length nums == 0 -> -1
                    | val == target -> mid
                    | val > target -> loop start (mid - 1)
                    | otherwise -> loop (mid + 1) end

search :: Vector Int -> Int -> Int
search nums target = runST $ do
    startRef <- newSTRef 0
    endRef <- newSTRef (V.length nums - 1)
    let loop = do
            i <- readSTRef startRef
            j <- readSTRef endRef
            if i > j
                then pure (-1)
                else do
                    let mid = (i + j) `div` 2
                    let val = nums V.! mid
                    if
                            | val == target -> pure mid
                            | val > target -> do
                                writeSTRef endRef (mid - 1)
                                loop
                            | otherwise -> do
                                writeSTRef startRef (mid + 1)
                                loop
    loop
