
import Debug.Trace


merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | y < x = y : merge ys (x:xs)
  | x == y = x : merge xs ys 
merged :: [Int]
merged = merge [2,5,8] [1,5,6,7]

main :: IO () 
