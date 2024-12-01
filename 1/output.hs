import Data.List

main = do
        contents <- readFile "input.txt"
        print "Part 1: "
        print . getTotalDifference . zipNestedLists . sortLists . splitAlternates . map readInt . words $ contents
        print "Part 2: "
        print . getSimilarityScore . sortLists . splitAlternates . map readInt . words $ contents

readInt :: String -> Int
readInt = read

splitAlternates :: [a] -> [[a]]
splitAlternates [] = [[], []]
splitAlternates (x:y:xs) = [x : xs1, y : xs2]
  where
    [xs1, xs2] = splitAlternates xs

sortLists ::[[Int]] -> [[Int]]
sortLists [[], []] = [[], []]
sortLists [x, y] = [sort x, sort y]

zipNestedLists :: [[a]] -> [(a, a)]
zipNestedLists [xs, ys] = zip xs ys

getTotalDifference :: [(Int, Int)] -> Int
getTotalDifference [] = 0
getTotalDifference (x:xs) = if fst x > snd x then fst x - snd x + getTotalDifference xs else snd x - fst x + getTotalDifference xs

getSimilarityScore :: [[Int]] -> Int
getSimilarityScore [] = 0
getSimilarityScore [[], []] = 0
getSimilarityScore [[], y] = 0
getSimilarityScore [x, []] = 0
getSimilarityScore [(x:xs), y] = x * (length . filter (== x)) y + getSimilarityScore [xs, y]
getSimilarityScore _ = error "something bad happened"
