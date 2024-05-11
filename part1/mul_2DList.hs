sumi :: [[Int]] -> Int
sumi [] = 1
sumi (x:xs) = product x * sumi xs

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs
main = do 
  let tmp1 = sumi [[],[],[]]
      tmp2 = sumi [[1],[1,2,3],[5]]
      tmp3 = sumi [[],[1,2,3],[5]]
      
      tmp4 = flatten [[1],[],[]]
      tmp5 = flatten [[1,1],[2,1],[5]]
      tmp6 = flatten [[],[1,2,3],[4]]

  print (tmp1)
  print (tmp2)
  print (tmp3)
  print (tmp4)
  print (tmp5)
  print (tmp6)
