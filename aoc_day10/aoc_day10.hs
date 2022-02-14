import Data.Maybe (catMaybes)
import Data.List (sort)

main :: IO()
main = do
    lines2 <- readLines "aoc_day10.txt"
    
    let corruptedChars = catMaybes $ map (\line -> isCorrupted line []) lines2
    print $ getPoints 0 corruptedChars

    let corruptedLines = map (\line -> isIncompleted line []) $ filter (\line -> isCorrupted line [] == Nothing ) lines2
    let incompletePoints = filter (\x -> x /= 0) $ map (getPoints2 0) corruptedLines
    print $ head $ middle $ sort incompletePoints

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

isCorrupted [] _ = Nothing
isCorrupted (x : xn) stack | isOpener x = isCorrupted xn (x : stack)
                           | otherwise = if canClose x stack then isCorrupted xn (tail stack)
                                                             else Just x

isIncompleted [] stack = stack
isIncompleted (x : xn) stack | isOpener x = isIncompleted xn (x : stack)
                             | otherwise = if canClose x stack then isIncompleted xn (tail stack)
                                                               else isIncompleted xn stack

getPoints :: Int -> String -> Int
getPoints points [] = points
getPoints points (')' : xn) = getPoints (points + 3) xn
getPoints points (']' : xn) = getPoints (points + 57) xn
getPoints points ('}' : xn) = getPoints (points + 1197) xn
getPoints points ('>' : xn) = getPoints (points + 25137) xn

getPoints2 :: Int -> String -> Int
getPoints2 points [] = points
getPoints2 points ('(' : xn) = getPoints2 (points * 5 + 1) xn
getPoints2 points ('[' : xn) = getPoints2 (points * 5 + 2) xn
getPoints2 points ('{' : xn) = getPoints2 (points * 5 + 3) xn
getPoints2 points ('<' : xn) = getPoints2 (points * 5 + 4) xn

isOpener :: Char -> Bool
isOpener = (`elem` "([{<")

canClose :: Char -> [Char] -> Bool
canClose ')' ('(' : _) = True
canClose ']' ('[' : _) = True
canClose '}' ('{' : _) = True
canClose '>' ('<' : _) = True
canClose _ _ = False

middle :: [a] -> [a]
middle l@(_:_:_:_) = middle $ tail $ init l
middle l = l