{-# OPTIONS_GHC -Wall -Werror #-}
-- http://book.realworldhaskell.org/read/testing-and-quality-assurance.html

import qualified Sort as S
import Data.List ((\\))
import Test.QuickCheck

prop_idempotent :: ([Integer] -> [Integer]) -> [Integer] -> Bool
prop_idempotent sort xs = sort (sort xs) == sort xs

prop_minimum :: ([Integer] -> [Integer]) -> [Integer] -> Property
prop_minimum sort xs =
  not (null xs) ==>
    head (sort xs) == minimum xs

prop_ordered :: ([Integer] -> [Integer]) -> [Integer] -> Bool
prop_ordered sort xs = ordered (sort xs)
  where
    ordered []       = True
    ordered [_]      = True
    ordered (x:y:zs) = x <= y && ordered (y:zs)

prop_permutation :: ([Integer] -> [Integer]) -> [Integer] -> Bool
prop_permutation sort xs = permutation xs (sort xs)
  where
    permutation ys zs = null (ys \\ zs) && null (zs \\ ys)

prop_maximum :: ([Integer] -> [Integer]) -> [Integer] -> Property
prop_maximum sort xs =
  not (null xs) ==>
    last (sort xs) == maximum xs

prop_append :: ([Integer] -> [Integer]) -> [Integer] -> [Integer] -> Property
prop_append sort xs ys =
  not (null xs) ==>
  not (null ys) ==>
    head (sort (xs ++ ys)) == min (minimum xs) (minimum ys)

main :: IO ()
main = do
  quickCheck $ prop_idempotent  S.qsort
  quickCheck $ prop_idempotent  S.selectionsort
  quickCheck $ prop_minimum     S.qsort
  quickCheck $ prop_minimum     S.selectionsort
  quickCheck $ prop_ordered     S.qsort
  quickCheck $ prop_ordered     S.selectionsort
  quickCheck $ prop_permutation S.qsort
  quickCheck $ prop_permutation S.selectionsort
  quickCheck $ prop_maximum     S.qsort
  quickCheck $ prop_maximum     S.selectionsort
  quickCheck $ prop_append      S.qsort
  quickCheck $ prop_append      S.selectionsort
