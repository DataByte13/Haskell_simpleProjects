is_primeNumber  :: Int -> Bool 
is_primeNumber  n = 
  if length [x | x <- [2,3..n-1] , mod n x == 0] > 0
    then False
  else True 



get_n :: Int -> [Int]
get_n n = [x | x <- [2..n-1] , is_primeNumber x == True]
main = do 
  let tmp1 = get_n 2
      tmp2 = get_n 3
      tmp3 = get_n 4
      tmp4 = get_n 30
      tmp5 = get_n 100
  print (tmp1)
  print (tmp2)
  print (tmp3)
  print (tmp4)
  print (tmp5)
