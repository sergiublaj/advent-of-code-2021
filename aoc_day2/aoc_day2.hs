main :: IO()
main = do
    lines2 <- readLines "aoc_day2.txt"

    let (horizontal, depth) = getProduct lines2 0 0
    print $ horizontal * depth

    let (horizontal2, depth2) = getAim lines2 0 0 0
    print $ horizontal2 * depth2

readLines :: FilePath -> IO [(String, Int)]
readLines path = do
    contents <- readFile path
    return $ (convert . words) contents

convert :: [String] -> [(String, Int)]
convert [] = []
convert [_] = []
convert (x1 : x2 : xn) = (x1, read x2 :: Int) : convert xn

getProduct :: Num a => [([Char], a)] -> a -> a -> (a, a)
getProduct [] horizontal depth = (horizontal, depth)
getProduct (("forward", x) : xn) horizontal depth = getProduct xn (horizontal + x) depth
getProduct (("up", x) : xn) horizontal depth = getProduct xn horizontal (depth - x)
getProduct (("down", x) : xn) horizontal depth = getProduct xn horizontal (depth + x)

getAim :: Num a => [([Char], a)] -> a -> a -> a -> (a, a)
getAim [] _ horizontal depth = (horizontal, depth)
getAim (("forward", x) : xn) aim horizontal depth = getAim xn aim (horizontal + x) (depth + x * aim)
getAim (("up", x) : xn) aim horizontal depth = getAim xn (aim - x) horizontal depth
getAim (("down", x) : xn) aim horizontal depth = getAim xn (aim + x) horizontal depth