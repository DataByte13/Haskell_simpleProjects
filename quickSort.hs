partition :: Int -> Int -> Int -> Int-> [Int] -> [Int]
partition idx leftwall high pivotIdx arr = 
  let piv = arr !! pivotIdx
  in if idx == high 
        then quick (take (leftwall+1) ( swap pivotIdx (leftwall+1) arr))++ [piv] ++ quick (drop (leftwall+2) ( swap pivotIdx (leftwall+1) arr) )
        else if (arr !! idx ) < (piv)
          then  partition (idx+1) (leftwall+1) high pivotIdx (swap idx (leftwall+1) arr) 
          else if (arr !! idx ) > (piv)
            then partition (idx+1) leftwall high pivotIdx arr
            else [-1] 
---partition :: Int -> Int -> Int -> Int-> [Int] -> [Int]
--partition idx leftwall high pivotIdx arr =
--  let piv = arr !! pivotIdx 
--  in case idx of  
--    idx'|  idx == (length arr - 1) ->  arr
--        |  arr !! idx' > piv -> partition (idx'+1) leftwall high pivotIdx arr 
--        |  arr !! idx' <  piv -> partition (idx'+1) (leftwall+1) high pivotIdx (swap idx' leftwall arr)
--    --    | idx' == high -> swap pivotIdx leftwall arr ++ quick (take (leftwall - 1) arr) ++ quick (drop leftwall arr)
quick :: [Int] -> [Int]
quick [] = []
quick arr = partition 0  (-1) (length arr - 1) (length arr - 1) arr

swap :: Int -> Int -> [Int] -> [Int]
swap i j xs
  | i == j = xs
  | otherwise =
    let ielement = xs !! (min i j)
        jelement = xs !! (max i j)
        before = take (min i j) xs
        middle = drop ((min i j)+1) (take (max i j) xs)
        end = drop (max i j + 1) xs
    in before ++ [jelement] ++ middle ++ [ielement] ++ end

-- Define the list you want to sort
unsortedList :: [Int]
unsortedList = [4, 2, 7, 1, 9, 3, 6, 5, 8]

-- Sort the list using your quicksort function
sortedList :: [Int]
sortedList = quick unsortedList 

displayInput :: Show a => a -> IO ()
displayInput x = putStrLn ("Input: " ++ show x)

-- Display the sorted list
main :: IO ()
main = print sortedList

