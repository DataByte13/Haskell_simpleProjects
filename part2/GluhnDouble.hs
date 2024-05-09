import Text.Read (readMaybe)
luhnDouble :: Int -> Int 
luhnDouble x
  | x*2 < 9 = x*2
  | otherwise = (x*2-9)
is_valid :: [Char] -> Bool 
is_valid [] = False
is_valid cardId =
  let evenIdx = [x | (x ,i) <- zip cardId [0..] , odd i]
      oddIdx = [x | (x ,i) <- zip cardId [0..] , even i]
      evencal =  map luhnDouble (map (\ch -> read [ch] :: Int) evenIdx)
      oddcal = map (\ch -> read [ch] :: Int) oddIdx
      sumi = sum evencal + sum oddcal
  in if mod sumi 10 == 0 then True else False
