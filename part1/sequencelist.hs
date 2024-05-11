
import Debug.Trace
seqList = 0 : [ 1/fromIntegral b | b <- [1,2..]]
seqListStr = "0" : [ "1" ++ "/" ++ (show b) | b <- [1,2..]]
moList = 1 : [ b*(b+1)/fromIntegral 2 | b <- [1,2..]]

main = do 
  let tmp0 = take 0 seqList
      tmp1 = take 1 seqList 
      tmp2 = take 5 seqList 
      tmp3 = take 10 seqList 
      tmp4 = take 0 seqListStr 
      tmp5 = take 1 seqListStr  
      tmp7 = take 5 seqListStr 
      tmp8 = take 10 seqListStr
      
      tmp9 = take 0  moList 
      tmp10 = take 1  moList 
      tmp11 = take 5  moList
      tmp12 = take 10 moList

  print (tmp0)
  print (tmp1)
  print (tmp2)
  print (tmp3)
  print (tmp4)
  print (tmp5)
  print (tmp7)
  print (tmp8)
  print (tmp9)
  print (tmp10)
  print (tmp11)
  print (tmp12)
