import Data.List
import Data.Char
import qualified Data.Trie as Trie
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

nonWordChars = "\n ?.,/:;!@#$1234567890()-_=+[]{}|\\\t"
maxWidth = 80
maxcompareWordLength = 15
histogramChar = 'x'

main :: IO ()
main = do
       contents <- {-# SCC "getContents" #-} LBS.getContents
       let lower = {-# SCC "toLower" #-} LBS.map toLower contents
       let wordsList = {-# SCC "split" #-} LBS.splitWith notWord lower
       let trie = {-# SCC "buildtrie" #-} buildTrie wordsList
       let wordsCount = {-# SCC "trieToList" #-} Trie.toList trie
       let histogram = {-# SCC "toHistogram" #-} wordCountToHistogram wordsCount
       BS.putStrLn (BS.unlines histogram)

--       let sorted = {-# SCC "sort" #-} sort wordsList
--       let wordsCount = {-# SCC "getCount" #-} getCount sorted

buildTrie :: [LBS.ByteString] -> Trie.Trie Int
buildTrie = foldl' insertTrie Trie.empty

insertTrie :: Trie.Trie Int -> LBS.ByteString -> Trie.Trie Int
insertTrie trie val =
     let strictVal = safeGet $ LBS.toChunks val in
         case Trie.lookup strictVal trie of
            Just i -> Trie.insert strictVal (i + 1) trie
            Nothing -> Trie.insert strictVal 1 trie

safeGet :: [BS.ByteString] -> BS.ByteString
safeGet [x] = x
safeGet _ = BS.empty

notWord :: Char -> Bool
notWord c = BS.elem c nonWordCharsBS
nonWordCharsBS = BS.pack nonWordChars

format :: BS.ByteString -> Int -> BS.ByteString
format str maxLen 
    | bslen str > maxLen = BS.concat [bstake (maxLen - bslen dots) str, dots]
    | otherwise = BS.concat [bsreplicate (maxLen - bslen str) ' ', str]
        where dots = BS.pack ".." 


wordCountToHistogram :: [(BS.ByteString, Int)] -> [BS.ByteString]
wordCountToHistogram count = histogram where
    sorted = sortBy compareFreqLen (dropEmpty count)
    maxcompareWordLen = min (bslen . fst $ maximumBy compareWordLen count) maxcompareWordLength
    maxHistogramLen = snd $ last sorted
    histogram =  fmap toHistogramLine sorted
    toHistogramLine (word, c) = BS.concat [format word maxcompareWordLen, BS.pack ": " , bsreplicate histoLen histogramChar] where
        histoLen = max (div (maxHistoLen * c) maxHistogramLen) 1
    maxHistoLen = maxWidth - maxcompareWordLen - 2

dropEmpty :: [(BS.ByteString, Int)] -> [(BS.ByteString, Int)]
dropEmpty (x:xs) | bslen (fst x) == 0 = xs
                 | otherwise = x : dropEmpty xs
dropEmpty x = x
        
compareWordLen :: (BS.ByteString, Int) -> (BS.ByteString, Int) -> Ordering
compareWordLen (a,_) (b,_) | diff < 0 = LT
                           | diff > 0 = GT
                           | otherwise = EQ
                               where diff = bslen a - bslen b

compareFreqLen :: (BS.ByteString, Int) -> (BS.ByteString, Int) -> Ordering
compareFreqLen (_,a) (_,b) | a < b = LT
                           | a > b = GT
                           | otherwise = EQ

getCount :: [BS.ByteString] -> [(BS.ByteString, Int)]
getCount = getCount' [] 1 where 
    getCount' acc c [w] = (w,c) : acc
    getCount' acc c (w:w':ws) | w == w' = getCount' acc (c+1) (w':ws)
                              | otherwise = getCount' ((w,c):acc) 1 (w':ws)
    getCount' _ _ _ = []

bslen bs = fromIntegral (BS.length bs)
bsreplicate c bs = BS.replicate (fromIntegral c) bs
bstake c bs =  BS.take (fromIntegral c) bs
