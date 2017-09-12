{-# OPTIONS_GHC -Wall -Werror #-}
module Sort where

import Data.List (partition)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =
  let (lt, ge) = partition (< x) xs
  in qsort lt ++ [x] ++ qsort ge

selectionsort :: Ord a => [a] -> [a]
selectionsort [] = []
selectionsort xs =
  let m = minimum xs  -- Former `listmin`. It could be implemented as
                      -- minimum (x:xs) = foldr min x xs
      (lhs, rhs) = partition (== m) xs
  in lhs ++ selectionsort rhs
