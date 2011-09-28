import Data.List
import Data.Char
import qualified Data.Trie as Trie
import Control.Monad
import qualified Data.ByteString.Char8 as BS

nonWordChars = "\n ?.,/:;!@#$1234567890()-_=+[]{}|\\\t"
maxWidth = 80
maxWordLength = 15
histogramChar = 'x'

main :: IO ()
main = do
       contents <- {-# SCC "getcontents" #-} BS.getContents
       let lower = {-# SCC "toLower" #-} BS.map toLower contents
       let wordsList = {-# SCC "split" #-} BS.splitWith notWord lower
--       let sorted = {-# SCC "sort" #-} sort wordsList
--       let wordsCount = {-# SCC "getCount" #-} getCount sorted
       let trie = {-# SCC "buildtree" #-} buildTrie wordsList
       let wordsCount = {-# SCC "trieToList" #-} Trie.toList trie
       let histogram = {-# SCC "tohisto" #-} wordCountToHistogram wordsCount
       BS.putStrLn (BS.unlines histogram)

buildTrie :: [BS.ByteString] -> Trie.Trie Int
buildTrie = foldl' insertTrie Trie.empty

insertTrie :: Trie.Trie Int -> BS.ByteString -> Trie.Trie Int
insertTrie trie val = case Trie.lookup val trie of
                    Just i -> Trie.insert val (i + 1) trie
                    Nothing -> Trie.insert val 1 trie


notWord :: Char -> Bool
notWord c = BS.elem c (BS.pack nonWordChars)

format :: BS.ByteString -> Int -> BS.ByteString
format str maxLen 
    | BS.length str > maxLen = BS.concat [BS.take (maxLen - BS.length dots) str, dots]
    | otherwise = BS.concat [BS.replicate (maxLen - BS.length str) ' ', str]
        where dots = BS.pack ".." 

wordCountToHistogram :: [(BS.ByteString, Int)] -> [BS.ByteString]
wordCountToHistogram count = histogram where
    sorted = sortBy sortPair (dropEmpty count)
    maxWordLen = min (BS.length . fst $ maximumBy wordLen count) maxWordLength
    maxHistogramLen = snd $ last sorted
    histogram =  fmap toHistogramLine sorted
    toHistogramLine (word, c) = BS.concat [format word maxWordLen, BS.pack ": " , BS.replicate histoLen histogramChar] where
        histoLen = max (div (maxHistoLen * c) maxHistogramLen) 1
    maxHistoLen = maxWidth - maxWordLen - 2

dropEmpty :: [(BS.ByteString, Int)] -> [(BS.ByteString, Int)]
dropEmpty (x:xs) | BS.length (fst x) == 0 = xs
                 | otherwise = x : dropEmpty xs
dropEmpty x = x
        

wordLen :: (BS.ByteString, Int) -> (BS.ByteString, Int) -> Ordering
wordLen (a,_) (b,_) | diff < 0 = LT
                   | diff > 0 = GT
                   | otherwise = EQ
                   where diff = BS.length a - BS.length b

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
