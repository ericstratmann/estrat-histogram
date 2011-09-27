import Data.List
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
       contents <- BS.getContents
       let wordsList = BS.splitWith notWord contents
       let wordsCount = getCount (sort wordsList)
       let histogram = wordCountToHistogram wordsCount
       BS.putStrLn (BS.unlines histogram)

notWord :: Char -> Bool
notWord c = BS.elem c (BS.pack "\n ?.,/;!")

format :: BS.ByteString -> Int -> BS.ByteString
format str maxLen 
    | BS.length str > maxLen = BS.concat [BS.take (maxLen - BS.length dots) str, dots]
    | otherwise = BS.concat [BS.replicate (maxLen - BS.length str) ' ', str]
        where dots = BS.pack("..")

wordCountToHistogram :: [(BS.ByteString, Int)] -> [BS.ByteString]
wordCountToHistogram count = histogram where
    sorted = sortBy sortPair (dropEmpty count)
    maxWordLen = min (BS.length $ fst $ maximumBy wordLen count) 15
    maxHistogramLen = snd $ last sorted
    histogram =  map toHistogramLine sorted where
        toHistogramLine (word, c) =
            BS.concat [(format word maxWordLen), BS.pack(": "), BS.replicate histoLen 'x']
                where histoLen = max (div (60 * c) maxHistogramLen) 1

dropEmpty :: [(BS.ByteString, Int)] -> [(BS.ByteString, Int)]
dropEmpty (x:xs) | BS.length (fst x) == 0 = xs
                 | otherwise = x : dropEmpty xs
dropEmpty _ = []
        

wordLen :: (BS.ByteString, Int) -> (BS.ByteString, Int) -> Ordering
wordLen (a,_) (b,_) | diff < 0 = LT
                   | diff > 0 = GT
                   | diff == 0 = EQ
                   where diff = BS.length a - BS.length b
wordLen _ _ = EQ

sortPair :: (BS.ByteString, Int) -> (BS.ByteString, Int) -> Ordering
sortPair (_,a) (_,b) | a < b = LT
                     | a > b = GT
                     | otherwise = EQ

getCount :: [BS.ByteString] -> [(BS.ByteString, Int)]
getCount = getCount' [] 1 where 
    getCount' acc c [w] = (w,c) : acc
    getCount' acc c (w:w':ws) | w == w' = getCount' acc (c+1) (w':ws)
                              | otherwise = getCount' ((w,c):acc) 1 (w':ws)
    getCount' _ _ _ = []
