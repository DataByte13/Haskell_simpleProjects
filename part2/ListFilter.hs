zfilter :: [[Char]] -> Int ->  [([Char],Int)]
zfilter [] _  = []
zfilter chList size= [ (x , i) | (x , i) <- ziping chList , i > size ]

ziping :: [[Char]] -> [([Char],Int)]
ziping [] = []
--ziping [[]] = [([],0)]
ziping (c:cs) = (c,length c) : ziping cs
main = do 
  let tmp1 = zfilter ["123", "1234", "12", "12345"] 1 
      tmp2 = zfilter ["123", "1234", "12", "12345"] 3 
      tmp3 = zfilter ["123", "1234", "12", "12345"] 5 
  
  print (tmp1)
  print (tmp2)
  print (tmp3)
