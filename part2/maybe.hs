f :: Int -> Maybe Int
f 0 = Nothing
f x = Just x

maybeSum :: Maybe Int -> Maybe Int -> Maybe Int 
maybeSum (Just x) (Just y) = Just (x+y)
maybeSum _ _ = Nothing

main = do 
  let tmp1 =maybeSum Nothing Nothing
      tmp2 =maybeSum Nothing (Just 5)
      tmp3 =maybeSum (Just 3) Nothing
      tmp4 =maybeSum (Just 7) (Just 8)
  
  print (tmp1)
  print (tmp2)
  print (tmp3)
  print (tmp4)
