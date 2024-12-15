import Data.List

main = do
        contents <- readFile "input.txt"
        print . countSafeReports 0 . map readIntList . map words . lines $ contents

readIntList :: [String] -> [Int]
readIntList [] = []
readIntList x = map readInt x

readInt :: String -> Int
readInt = read

countSafeReports :: Int -> [[Int]] -> Int
countSafeReports count (x:xs)        | xs == [] = count
        | determineSafety x == True = countSafeReports (count + 1) xs
        | determineSafety x == False = countSafeReports count xs

determineSafety :: [Int] -> Bool
determineSafety [] = False
determineSafety x = consistentDirection (getInitialDirection x) x && maxThreeDiff x

getInitialDirection :: [Int] -> (Int -> Int -> Bool)
getInitialDirection (x:xs)
        | x > head xs = (>)
        | x < head xs = (<)
        | x == head xs = (==)

consistentDirection :: (Int -> Int -> Bool) -> [Int] -> Bool
consistentDirection op (x:xs)
        | xs == [] = True
        | x `op` head xs == False = False
        | x `op` head xs == True = consistentDirection op xs

maxThreeDiff :: [Int] -> Bool
maxThreeDiff (x:xs)
        | xs == [] = True
        | x - head xs > 3 || x - head xs < -3 || x - head xs == 0 = False
        | otherwise = maxThreeDiff xs
