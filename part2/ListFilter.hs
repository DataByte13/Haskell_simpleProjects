zfilter :: [[Char]] -> Int ->  [([Char],Int)]
zfilter [] _  = []
zfilter chList size= [ (x , i) | (x , i) <- ziping chList , i > size ]

ziping :: [[Char]] -> [([Char],Int)]
ziping [] = []
--ziping [[]] = [([],0)]
ziping (c:cs) = (c,length c) : ziping cs
