import Data.Char
import Data.List
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
       contents <- BS.getContents
       let wordsList = BS.splitWith notWord contents
       let sorted = sort wordsList
       let count = getCount sorted
       let histogram = wordCountToHistogram count
       BS.putStrLn (BS.unlines histogram)

notWord :: Char -> Bool
notWord c = BS.elem c (BS.pack "?.,/;!")


wordCountToHistogram :: [(BS.ByteString, Integer)] -> [BS.ByteString]
wordCountToHistogram count = histogram where
    sorted = sortBy sortPair count
    histogram =  map toHistogramLine sorted
        
toHistogramLine :: (BS.ByteString, Integer) -> BS.ByteString
toHistogramLine (word, c) =  word -- ++ ": " ++ take (fromIntegral c) (repeat 'x')

sortPair :: (BS.ByteString, Integer) -> (BS.ByteString, Integer) -> Ordering
sortPair (_,a) (_,b) | a < b = LT
                     | a > b = GT
                     | otherwise = EQ

getCount :: [BS.ByteString] -> [(BS.ByteString, Integer)]
getCount = getCount' [] 1 where 
    getCount' acc c [w] = (w,c) : acc
    getCount' acc c (w:w':ws) | w == w' = getCount' acc (c+1) (w':ws)
                              | otherwise = getCount' ((w,c):acc) 1 (w':ws)
    getCount' _ _ _ = []

isWordSep c = isPunctuation c || isSpace c
