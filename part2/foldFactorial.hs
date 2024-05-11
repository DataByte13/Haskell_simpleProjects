foldFac :: Int -> Int 
foldFac 1 = 1
foldFac 0 = 0
foldFac x = 
  let listOf = [1..x]
  in  foldl (*) 1 listOf
main = do 
  let tmp1 = foldFac 0
      tmp2 = foldFac 1
      tmp3 = foldFac 5
  
  print (tmp1)
  print (tmp2)
  print (tmp3)
