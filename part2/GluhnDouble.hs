import Text.Read (readMaybe)
luhnDouble :: Int -> Int 
luhnDouble x
  | x*2 < 9 = x*2
  | otherwise = (x*2-9)
is_valid :: [Char] -> Bool 
is_valid [] = False
is_valid cardId =
  let oddIdx = [x | (x ,i) <- zip cardId [0..] , odd i]
      evenIdx = [x | (x ,i) <- zip cardId [0..] , even i]
      evencal =  map luhnDouble (map (\ch -> read [ch] :: Int) evenIdx)
      oddcal = map (\ch -> read [ch] :: Int) oddIdx
      sumi = sum evencal + sum oddcal
  in if mod sumi 10 == 0 then True else False
main = do 
  let tmp1 = is_valid "6239" 
      tmp2 = is_valid "1008432272879781"
      tmp3 = is_valid "6104337882900792"
      tmp4 = is_valid "5894631538912916"
  print (tmp1)
  print (tmp2)
  print (tmp3)
  print (tmp4)
