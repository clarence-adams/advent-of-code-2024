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
countSafeReports count (x:xs)
        | safe == True && xs == [] = count + 1
        | safe == False && xs == [] = count
        | safe == True = countSafeReports (count + 1) xs
        | safe == False = countSafeReports count xs
        where safe = determineSafety x

determineSafety :: [Int] -> Bool
determineSafety [] = False
determineSafety x = safety
        where safety = consistentDirection (getInitialDirection x) x x 1 0

getInitialDirection :: [Int] -> (Int -> Int -> Bool)
getInitialDirection (x:xs)
        | x > head xs = (>)
        | x < head xs = (<)
        | x == head xs = (==)

consistentDirection :: (Int -> Int -> Bool) -> [Int] -> [Int] -> Int -> Int -> Bool
consistentDirection op (x:xs) y pd index
        | xs == [] = maxThreeDiff'
        | correctDirection == False && lastElement == False = consistentDirection'
        | correctDirection == False && lastElement == True = False
        | correctDirection == True && lastElement == True && xs == [] = maxThreeDiff'
        | otherwise = consistentDirection op xs y pd index
        where correctDirection = (x `op` head xs) && 0 `op` 0 == False
              pdList = splice y index
              consistentDirection' = consistentDirection (getInitialDirection pdList) pdList y 0 (index + 1)
              lastElement = index + 1 > length y
              maxThreeDiff' = if (maxThreeDiff maxThreeList maxThreeList pd 0) == True then True 
                              else if lastElement then False
                              else consistentDirection'

              maxThreeList = if index == 0 && pd == 0 then y 
                             else if lastElement == True then splice y index 
                             else splice y (index - 1)

maxThreeDiff :: [Int] -> [Int] -> Int -> Int -> Bool
maxThreeDiff (x:xs) y pd index
        | xs == [] = True
        | incorrectDiff && lastElement == False && pd > 0 = maxThreeDiff'
        | incorrectDiff && lastElement == False && pd < 1 = False
        | incorrectDiff && lastElement == True = False
        | otherwise = maxThreeDiff xs y pd index 
        where incorrectDiff = x - head xs > 3 || x - head xs < -3 || x - head xs == 0
              pdList = splice y index
              lastElement = index + 1 > length y
              maxThreeDiff' = maxThreeDiff pdList y pd (index + 1)

splice :: [Int] -> Int -> [Int]
splice x index
        | null x = []
        | index == length x = take (index - 1) x
        | index > length x = error "Index doesn't exist"
        | otherwise = splitList
        where splitList = (take index x) ++ (drop (index + 1) x)
