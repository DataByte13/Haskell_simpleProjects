seqList = 1 : [ 1/fromIntegral b | b <- [1,2..]]
seqListStr = "1" : [ "1" ++ "/" ++ (show b) | b <- [1,2..]]
moList = 1 : [ b*(b+1)/fromIntegral 2 | b <- [1,2..]]

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | y < x = y : merge ys (x:xs)
  | x == y = x : merge xs ys 
merged :: [Int]
merged = merge [2,5,8] [1,5,6,7]

luhnDouble :: Int -> Int 
luhnDouble x
  | x*2 < 9 = x*2
  | otherwise = (x*2-9)
tmp = luhnDouble 6

partition :: Int -> Int -> Int -> Int-> [Int] -> [Int]
partition idx leftwall high pivotIdx A =
  let piv = A !! pivotIdx 
  in case idx of  
    idx'|  A !! idx' > piv -> partition (idx'+1) leftwall high pivotIdx A 
        |  A !! idx' < piv -> partition (idx'+1) (leftwall+1) high pivotIdx (swap idx' leftwall A)
        | idx' == high -> swap pivotIdx leftwall A ++ quick (take (leftwall - 1) A) ++ quick (drop leftwall A)

quick :: [Int] -> [Int]
quick [] = []
quick arr@(pivot:reset) = partition 0 0 (length arr-1) 0 arr

swap :: Int -> Int -> [Int] -> [Int]
swap i j xs
  | i == j = xs
  | otherwise =
    let ielement = xs !! (min i j)
        jelement = xs !! (max i j)
        before = take (min i j) xs
        middle = take (abs(i - j)-1) (drop ((min i j)+1)xs)
        end = drop (max i j + 1) xs
    in before ++ [jelement] ++ middle ++ [ielement] ++ end

-- Define the list you want to sort
unsortedList :: [Int]
unsortedList = [4, 2, 7, 1, 9, 3, 6, 5, 8]

-- Sort the list using your quicksort function
sortedList :: [Int]
sortedList = quick unsortedList 0 (length unsortedList - 1)

-- Display the sorted list
main :: IO ()
main = print sortedList
--swap :: Int -> Int -> [Int] -> [Int]
--swap i j xs if i == j then xs else {
--     let ielemnt = xs !! (min i j)
--     let jelemnt = xs !! (max i j)
--     before = take (min i j) xs
--     middel = take (max i j) (drop (min i j) xs)
--     end = drop (max i j) xs
--    newlist = [before] ++ [jelemnt] ++ [middel] ++ [ielemnt] ++ [end]
--  }
--main :: IO()
--main = putStrLn "hello"
