
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
main = do 
  let tmp1 = merge [][1]
      tmp2 = merge [1][]
      tmp3 = merge [11][1..10]
      tmp4 = merge [2,3,7][1,5,10]
  print (tmp1)
  print (tmp2)
  print (tmp3)
  print (tmp4)
