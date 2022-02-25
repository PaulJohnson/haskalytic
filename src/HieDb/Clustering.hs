

{- |
Module              : HieDb.Clustering
Copyright           : Ⓒ 2022 Paul Johnson
License             : BSD3
Maintainer          : Paul Johnson

-}

module HieDb.Clustering (

) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S



{-
Assign each item to a singleton cluster. Compute distances between clusters. Merge two closest
clusters. Repeat until distance between the closest clusters exceeds a threshold.
-}


-- | The mode of a list of sets, defined as the most popular value.
setMode :: (Ord a) => [Set a] -> Set a
setMode ss = S.fromList $ take 1 freqList
   where
      freqs = M.fromListWith (+) $ map (,1) $ concatMap S.toList ss
      freqList = reverse $ map fst $ sortOn snd $ M.toList freqs


-- | List of distances between members of the set, sorted in ascending order of distance.
distanceList :: Ord d => (a -> a -> d) -> Set a -> [(d, (a,a))]
distanceList f xs = sortOn fst distances
   where
      pairs = ordPairs $ S.toAscList xs
      distances = map (\p -> (uncurry f p, p)) pairs


-- | If the argument is an ordered list then this returns every pair @(x,y)@ such that @x < y@
ordPairs :: [a] -> [(a,a)]
ordPairs [] = []
ordPairs (x1:xs) = [(x1,x2) | x2 <- xs] ++ ordPairs xs


-- | The Jaccard distance between two sets. Zero for equal sets, 1 for disjoint sets.
jaccardDistance :: (Ord a) => Set a -> Set a -> Double
jaccardDistance xs ys =
      if unionSize == 0
         then 0  -- Two empty sets are equal.
         else 1.0 - fromIntegral intersectionSize / fromIntegral unionSize
   where
      intersectionSize = S.size $ S.intersection xs ys
      unionSize = S.size $ S.union xs ys
