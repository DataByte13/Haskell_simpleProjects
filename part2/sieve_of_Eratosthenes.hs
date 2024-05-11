input_n :: Int -> Int  ->  [Int]
input_n 0 _ = []
input_n 1 _ = [1]
input_n s e = eSieve [s..e-1]

eSieve :: [Int] -> [Int]
eSieve [] = []
eSieve (n:ns) = n : eSieve [rn | rn <- ns , mod rn n /= 0]
main = do 
  let tmp1 = input_n 2 9 
  print (tmp1)

