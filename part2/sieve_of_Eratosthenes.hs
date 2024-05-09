input_n :: Int ->  [Int]
input_n 0 = []
input_n 1 = [1]
input_n n = eSieve [2..n-1]

eSieve :: [Int] -> [Int]
eSieve [] = []
eSieve (n:ns) = n : eSieve [rn | rn <- ns , mod rn n /= 0]
