import Data.List
import Data.Char
import qualified Data.Map as Trie
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

nonWordChars :: [Char]
nonWordChars = "\n ?.,/:;!@#$1234567890()-_=+[]{}|\\\t"

maxWidth :: Int
maxWidth = 80

maxcompareWordLength :: Int
maxcompareWordLength = 15

histogramChar :: Char
histogramChar = '#'

main :: IO ()
main = do
       args <- getArgs
       contents <- {-# SCC "getContents" #-} LBS.getContents
       let lower = {-# SCC "toLower" #-} LBS.map toLower contents
       let wordsList = {-# SCC "split" #-} LBS.splitWith notWord lower
       let trie = {-# SCC "buildtrie" #-} buildTrie wordsList
       let wordsCount = {-# SCC "trieToList" #-} Trie.toList trie
       let histogram = {-# SCC "toHistogram" #-} wordCountToHistogram wordsCount
       BS.putStrLn (BS.unlines histogram)

buildTrie :: [LBS.ByteString] -> Trie.Map BS.ByteString Int
buildTrie = foldl' insertTrie Trie.empty

insertTrie :: Trie.Map BS.ByteString Int -> LBS.ByteString -> Trie.Map BS.ByteString Int
insertTrie trie val =
     let strictVal = safeGet $ LBS.toChunks val in
        Trie.alter updateVal strictVal trie

updateVal :: Maybe Int -> Maybe Int
updateVal (Just i) = Just (i+1)
updateVal Nothing = Just 1

safeGet :: [BS.ByteString] -> BS.ByteString
safeGet [x] = x
safeGet _ = BS.empty

notWord :: Char -> Bool
notWord c = BS.elem c nonWordCharsBS

nonWordCharsBS :: BS.ByteString
nonWordCharsBS = BS.pack nonWordChars

format :: BS.ByteString -> Int -> BS.ByteString
format str maxLen 
    | BS.length str > maxLen = BS.concat [BS.take (maxLen - BS.length dots) str, dots]
    | otherwise = BS.concat [BS.replicate (maxLen - BS.length str) ' ', str]
        where dots = BS.pack ".." 


wordCountToHistogram :: [(BS.ByteString, Int)] -> [BS.ByteString]
wordCountToHistogram count = histogram where
    sorted = sortBy compareFreqLen (dropEmpty count)
    maxcompareWordLen = min (BS.length . fst $ maximumBy compareWordLen count) maxcompareWordLength
    maxHistogramLen = snd $ head sorted
    histogram =  fmap toHistogramLine sorted
    toHistogramLine (word, c) = BS.concat [format word maxcompareWordLen, BS.pack ": " , BS.replicate histoLen histogramChar] where
        histoLen = max (div (maxHistoLen * c) maxHistogramLen) 1
    maxHistoLen = maxWidth - maxcompareWordLen - 2

dropEmpty :: [(BS.ByteString, Int)] -> [(BS.ByteString, Int)]
dropEmpty (x:xs) | BS.length (fst x) == 0 = xs
                 | otherwise = x : dropEmpty xs
dropEmpty x = x
        
compareWordLen :: (BS.ByteString, Int) -> (BS.ByteString, Int) -> Ordering
compareWordLen (a,_) (b,_) | diff < 0 = LT
                           | diff > 0 = GT
                           | otherwise = EQ
                               where diff = BS.length a - BS.length b

compareFreqLen :: (BS.ByteString, Int) -> (BS.ByteString, Int) -> Ordering
compareFreqLen (_,a) (_,b) | a > b = LT
                           | a < b = GT
                           | otherwise = EQ
