import Data.List.Split
import Data.List (sort)

main :: IO()
main = do
    lines2 <- readLines "aoc_day7.txt"
    let strings = splitOn "," $ lines2 !! 0
    let numbers = parseInts strings
    
    let sortedNumbers = sort numbers
    let median = middle sortedNumbers !! 0
    print $ sum $ map(\x -> abs $ x - median) sortedNumbers

    let median21 = floor $ average numbers - 1
    let median22 = floor $ average numbers 
    let median23 = floor $ average numbers + 1
    let fuel21 = sum $ map(\x -> stepSum $ abs $ x  - median21) numbers
    let fuel22  = sum $ map(\x -> stepSum $ abs $ x  - median22) numbers
    let fuel23 = sum $ map(\x -> stepSum $ abs $ x  - median23) numbers
    print $ minimum [fuel21, fuel22, fuel23]

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseInts :: [String] -> [Int]
parseInts = map read

middle :: [a] -> [a]
middle l@(_:_:_:_) = middle $ tail $ init l
middle l = l

average :: (Real a, Fractional b) => [a] -> b
average xn = realToFrac (sum xn) / (fromIntegral $ length xn)

stepSum :: Int -> Int
stepSum x = x * (x + 1) `div` 2
