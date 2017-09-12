{-# OPTIONS_GHC -Wall -Werror #-}
module Sort where

import Data.List (partition)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =
  let (le, gt) = partition (<= x) xs
  in qsort le ++ [x] ++ qsort gt

selectionsort :: Ord a => [a] -> [a]
selectionsort [] = []
selectionsort l@(x:xs) =
  let m  = foldr min x xs   -- listmin
      l' = filter (/= m) l  -- [a | a <- l, a /= m]
  in m : selectionsort l'
