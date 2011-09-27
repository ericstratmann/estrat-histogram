import Data.Char
import Data.List

main :: IO ()
main = do
       contents <- getContents
       let wordsList = getWords contents
       let sorted = sort wordsList
       let count = getCount sorted
       let histogram = wordCountToHistogram count
       putStrLn histogram


wordCountToHistogram :: [(String, Integer)] -> String
wordCountToHistogram count = histogram where
    sorted = sortBy sortPair count
    histogram = unlines $ map toHistogramLine sorted
        
toHistogramLine :: (String, Integer) -> String
toHistogramLine (word, c) =  word ++ ": " ++ take (fromIntegral c) (repeat 'x')

sortPair :: (String, Integer) -> (String, Integer) -> Ordering
sortPair (_,a) (_,b) | a < b = LT
                     | a > b = GT
                     | otherwise = EQ

getCount :: [String] -> [(String, Integer)]
getCount = getCount' [] 1 where 
    getCount' acc c [w] = (w,c) : acc
    getCount' acc c (w:w':ws) | w == w' = getCount' acc (c+1) (w':ws)
                              | otherwise = getCount' ((w,c):acc) 1 (w':ws)
    getCount' _ _ _ = []


-- modified `words' from prelude
getWords :: String -> [String]
getWords s =  case dropWhile isWordSep s of
    "" -> []
    s' -> w : getWords s''
        where (w, s'') = break isWordSep s'

isWordSep :: Char -> Bool
isWordSep c = isPunctuation c || isSpace c
