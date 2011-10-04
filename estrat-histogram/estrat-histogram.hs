import System.Environment
import Data.List
import Data.Char
import Data.Map
import Data.Ord

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
    let wordMap = buildMap wordsList
    let wordsCount = toList wordMap
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
insertMap wordMap word = alter incrementMaybe word wordMap

-- Updates a Maybe to increment the word count by 1.
incrementMaybe :: Maybe Int -> Maybe Int
incrementMaybe (Just i) = Just (i+1)
incrementMaybe Nothing = Just 1

-- Given a list of (word, count) pairs, returns a list of strings that output
-- a histogram when printed.
wordCountToHistogram :: [(String, Int)] -> [String]
wordCountToHistogram wordCount = convertToASCII sortedWordLen maxWordLen where
    sortedWordCount = reverse $ sortBy (comparing snd) wordCount
    maxWordLen = getMaxWordLen sortedWordCount
    maxWordFreq = getMaxWordFreq sortedWordCount
    sortedWordLen = Data.List.filter notEmptyLine $ fmap countToLen sortedWordCount
    countToLen (word, len) = (word, div (maxHistogramLen * len) maxWordFreq)
    maxHistogramLen = maxWidth - maxWordLen - 2

-- Returns whether the histogram line would not be empty when printed
notEmptyLine :: (String, Int) -> Bool
notEmptyLine (_, len) = len > 0

-- Returns the length of the longest word to display
getMaxWordLen :: [(String, Int)] -> Int
getMaxWordLen sorted  = min (length . fst $ maximumBy (comparing (length . fst)) sorted) maxWordLength

-- Returns the largest word count in a list that is sorted max first
getMaxWordFreq :: [(String, Int)] -> Int
getMaxWordFreq = snd . head 

-- Generates ASCII text for each (word, len) pair
convertToASCII :: [(String, Int)] -> Int -> [String]
convertToASCII wordLens maxWordLen = fmap toHistogramLine wordLens where
    toHistogramLine (word, len) = format word maxWordLen ++ " " ++ replicate len histogramChar
 
-- Formats a word so that it will print nicely
format :: String -> Int -> String
format str maxLen 
    | length str > maxLen = take (maxLen - length dots) str ++ dots
    | otherwise = str ++ replicate (maxLen - length str) ' '
        where dots = ".." 
        
-- Returns a list of all words in the string. Adapted from `words' in the
-- Haskell Standard Prelude.
getWords :: String -> [String]
getWords s
  | findSpace == [] = []
  | otherwise = w : getWords s'' where
      (w, s'') = break isDelim findSpace
      findSpace = dropWhile isDelim s

-- Returns whether character is a delimiter character that separates words
isDelim :: Char -> Bool
isDelim c = c /= '\'' && (isPunctuation c || isSpace c)
