sumi :: [[Int]] -> Int
sumi [] = 0
sumi (x:xs) = sum x + sumi xs

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

