{-# LANGUAGE MultiWayIf #-}

module BinarySearch.BinarySearch () 
where

import Control.Monad (forM_)
import Control.Monad.Loops (whileM_)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Vector (Vector)
import qualified Data.Vector as V

search :: Vector Int -> Int -> Int
search nums target = runST $ do
    startRef <- newSTRef 0
    endRef   <- newSTRef (V.length nums - 1)
    let loop = do
          i <- readSTRef startRef
          j <- readSTRef endRef
          if i > j
            then pure (-1)
            else do
              let mid = (i + j) `div` 2
              let val = nums V.! mid
              if | val == target -> pure mid
                 | val > target  -> do
                    writeSTRef endRef (mid - 1)
                    loop
                 | otherwise -> do
                    writeSTRef startRef (mid + 1)
                    loop
    loop
