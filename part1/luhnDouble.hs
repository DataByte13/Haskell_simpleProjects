luhnDouble :: Int -> Int 
luhnDouble x
  | x > 9 = -1
  | x*2 < 9 = x*2
  | otherwise = (x*2-9)
tmp = luhnDouble 6
main = do 
  let tmp1 = luhnDouble 10 
      tmp2 = luhnDouble 1 
      tmp3 = luhnDouble 8
  print (tmp1)
  print (tmp2)
  print (tmp3)

