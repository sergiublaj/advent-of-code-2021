{-# LANGUAGE ParallelListComp #-}

import Data.List.Split
import Data.Map (fromListWith, toList)

main :: IO()
main = do
    lines2 <- readLines "aoc_day5.txt"
    let strings = map (\x -> splitOn " -> " x) lines2
    let numbers = parseInts $ concat $ map (\x -> splitOn "," x) $ concat strings
    
    let straightCoords = concat $ filter (\[x, y, z, w] -> isStraight x y z w) . chunksOf 4 $ numbers
    let allPoints = concat $ map (\[x, y, z, w] -> points x y z w) . chunksOf 4 $ straightCoords
    let frequencyCoords = frequency $ map (\[x, y] -> [x, y]) . chunksOf 2 $ allPoints
    print $ length $ filter (\(_, y) -> y > 1) frequencyCoords

    let goodCoords = concat $ filter (\[x, y, z, w] -> isStraight x y z w || isDiagonal x y z w) . chunksOf 4 $ numbers
    let allPoints2 = concat $ map (\[x, y, z, w] -> points x y z w) . chunksOf 4 $ goodCoords
    let frequencyCoords2 = frequency $ map (\[x, y] -> [x, y]) . chunksOf 2 $ allPoints2
    print $ length $ filter (\(_, y) -> y > 1) frequencyCoords2

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseInts :: [String] -> [Int]
parseInts = map read

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xn = toList (fromListWith (+) [(x, 1) | x <- xn])

isStraight :: Int -> Int -> Int -> Int -> Bool
isStraight x1 y1 x2 y2 = x1 == x2 || y1 == y2

isDiagonal :: Int -> Int -> Int -> Int -> Bool
isDiagonal x1 y1 x2 y2 = x1 - y1 == x2 - y2 || x1 + y1 == x2 + y2

points :: Int -> Int -> Int -> Int -> [Int]
points x1 y1 x2 y2
  | x1 == x2  = concat $ [[x1,y] | y <- range y1 y2]
  | y1 == y2  = concat $ [[x,y1] | x <- range x1 x2]
  | otherwise = concat $ [[x,y ] | x <- range x1 x2 | y <- range y1 y2]

range :: Int -> Int -> [Int]
range x y
  | x <= y    = [x, x + 1 .. y]
  | otherwise = [x, x - 1 .. y]