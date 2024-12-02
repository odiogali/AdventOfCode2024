module Main where

import System.IO

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (p : xs) = quicksort lesser ++ [p] ++ quicksort greater
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs

toNum :: [String] -> [Int]
toNum = map read

sumList :: [Int] -> Int
sumList [] = 0
sumList (x : xs) = x + sumList xs

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let splitContents = words contents

  let firstIndices = [0, 2 .. length splitContents - 1]
  let secondIndices = [1, 3 .. length splitContents]

  let list1 = [splitContents !! x | x <- firstIndices]
  let list2 = [splitContents !! x | x <- secondIndices]

  let numList1 = toNum list1
  let numList2 = toNum list2

  let sorted1 = quicksort numList1
  let sorted2 = quicksort numList2

  let diffList = zipWith (\x y -> abs (x - y)) sorted1 sorted2

  putStrLn "The sum of the differences is:"
  print (sumList diffList)

  hClose handle
