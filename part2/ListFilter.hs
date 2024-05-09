filter :: [[Char]] -> Int -> [[Char]]
filter [[]] = [[]]
filter chList size= [ (x , i) | (x , i) <- ziping chList , i > size ]

ziping :: 
