import System.Environment
import Data.List
import Data.Char
import Data.Map

-- This program outputs an ASCII histogram based on word frequncy

maxWidth :: Int
maxWidth = 80

maxWordLength :: Int
maxWordLength = 15

histogramChar :: Char
histogramChar = '#'

main :: IO ()
main = do
    input <- readInput
    let lower = fmap toLower input
    let wordsList = getWords lower
    let mapo = buildMap wordsList
    let wordsCount = toList mapo
    let histogram = wordCountToHistogram wordsCount
    putStrLn (unlines histogram)

-- Returns all files passed in as input or any data from STDIN.
readInput :: IO String
readInput = do
    args <- getArgs
    if Data.List.null args
        then 
            getContents
        else do
            fileData <- mapM readFile args
            return $ concat fileData

-- Given a list of words, builds a map containing a frequncy count of each word.
buildMap :: [String] -> Map String Int
buildMap = foldl' insertMap empty

-- Updates the word count in the map for the given word.
insertMap :: Map String Int -> String -> Map String Int
insertMap map val = alter updateVal val map

-- Updates a Maybe to increment the word count by 1.
updateVal :: Maybe Int -> Maybe Int
updateVal (Just i) = Just (i+1)
updateVal Nothing = Just 1

-- Given a list of (word, count) pairs, returns a list of strings that output
-- a histogram when printed.
wordCountToHistogram :: [(String, Int)] -> [String]
wordCountToHistogram wordCount = convertToASCII sortedWordLen maxWordLen where
    sortedWordCount = sortBy compareFreqLen wordCount
    maxWordLen = getMaxWordLen sortedWordCount
    maxWordFreq = getMaxWordFreq sortedWordCount
    sortedWordLen = Data.List.filter notEmptyLine $ fmap histoLen sortedWordCount
    histoLen (word, len) = (word ,div (maxHistogramLen * len) maxWordFreq)
    maxHistogramLen = maxWidth - maxWordLen - 1

-- Returns whether the histogram line would not be empty when printed
notEmptyLine :: (String, Int) -> Bool
notEmptyLine (word, len) = len > 0

-- Returns the length of the longest word
getMaxWordLen :: [(String, Int)] -> Int
getMaxWordLen sorted  = min (length . fst $ maximumBy compareWordLen sorted) maxWordLength

-- Returns the largest word count in a list that is sorted max first
getMaxWordFreq :: [(String, Int)] -> Int
getMaxWordFreq = snd . head 

-- Generates ASCII text for each (word, len) pair
convertToASCII :: [(String, Int)] -> Int -> [String]
convertToASCII wordLens maxWordLen = fmap toHistogramLine wordLens where
    toHistogramLine (word, len) = concat [format word maxWordLen, " " , replicate len histogramChar]
 
-- Formats a word so that it will print nicely
format :: String -> Int -> String
format str maxLen 
    | length str > maxLen = take (maxLen - length dots) str ++ dots
    | otherwise = str ++ replicate (maxLen - length str) ' '
        where dots = ".." 
        
-- Sorts (word, count) pairs by shortest word first
compareWordLen :: (String, Int) -> (String, Int) -> Ordering
compareWordLen (a,_) (b,_) | diff < 0 = LT
                           | diff > 0 = GT
                           | otherwise = EQ
                               where diff = length a - length b

-- Sorts (word, count) pairs by largest frequency first
compareFreqLen :: (String, Int) -> (String, Int) -> Ordering
compareFreqLen (_,a) (_,b) | a > b = LT
                           | a < b = GT
                           | otherwise = EQ

-- Returns a list of all words in the string. Adapted from `words' in the
-- Haskell Standard Prelude.
getWords s
  | findSpace == [] = []
  | otherwise = w : getWords s'' where
      (w, s'') = break isDelim findSpace
      findSpace = dropWhile isDelim s

-- Returns whether is a delimiter character that separates words
isDelim :: Char -> Bool
isDelim s = s /= '\'' && (isPunctuation s || isSpace s)
