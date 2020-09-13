module Util where

import Data.Char

mergeToTuples :: [a] -> [b] -> [(a, b)]
mergeToTuples [] [] = []
mergeToTuples (a : as) (b : bs) = (a, b) : mergeToTuples as bs
mergeToTuples _ _ = error "Fatal: mergeToTuples received lists of unequal length"

rowColToText :: (Int, Int) -> String
rowColToText (row, col) =
  [char] ++ (show (col + 1))
  where
    char = chr (ord 'A' + row)
