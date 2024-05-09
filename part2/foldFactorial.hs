foldFac :: Int -> Int 
foldFac 1 = 1
foldFac 0 = 0
foldFac x = 
  let listOf = [1..x]
  in  foldl (*) 1 listOf

