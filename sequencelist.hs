
import Debug.Trace

seqList = 1 : [ 1/fromIntegral b | b <- [1,2..]]
seqListStr = "1" : [ "1" ++ "/" ++ (show b) | b <- [1,2..]]
moList = 1 : [ b*(b+1)/fromIntegral 2 | b <- [1,2..]]

main :: IO ()
