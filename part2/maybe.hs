f :: Int -> Maybe Int
f 0 = Nothing
f x = Just x

maybeSum :: Maybe Int -> Maybe Int -> Maybe Int 
maybeSum (Just x) (Just y) = Just (x+y)
maybeSum _ _ = Nothing
