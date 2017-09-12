module Main where

import Sort

main :: IO ()
main = do
  let l = [9, 1, 8, 2, 7, 3, 6, 4, 5]
  print $ qsort l
  print $ selectionsort l
