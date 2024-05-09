luhnDouble :: Int -> Int 
luhnDouble x
  | x*2 < 9 = x*2
  | otherwise = (x*2-9)
tmp = luhnDouble 6
main :: IO ()


