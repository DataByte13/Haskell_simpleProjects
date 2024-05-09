is_primeNumber  :: Int -> Bool 
is_primeNumber  n = 
  if length [x | x <- [2,3..n-1] , mod n x == 0] > 0
    then False
  else True 



get_n :: Int -> [Int]
get_n n = [x | x <- [2..n-1] , is_primeNumber x == True]
