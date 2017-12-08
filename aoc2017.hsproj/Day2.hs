module Day2 where
  
import Text.Read (readMaybe)
  
stringToInt :: String -> Maybe Int
stringToInt = readMaybe

stringsToInts = mapM stringToInt
lineToInts = stringsToInts . words

dataTo2dArray :: String -> Maybe [[Int]]
dataTo2dArray = sequence . map lineToInts . lines

countDifference :: [Int] -> Int
countDifference row = foldl (+) 0 $ sequence [(foldl max 0), (foldl min 0)] row

countChecksum :: ([Int] -> Int) -> String -> Maybe Int
countChecksum mappingFn input = 
  fmap countSum table
  where table = dataTo2dArray input
        countSum t = foldl (+) 0 $ map mappingFn t

createGroups :: [a] -> [(a, a)]
createGroups [] = []
createGroups (x:xs) = map ((,) x) xs ++ createGroups xs

orderPair (a,b) = (max a b, min a b)

countDifference' :: [Int] -> Int
countDifference' intList =
  let combinations = map orderPair $ createGroups intList
      pairs = filter (\(h,l) -> h `mod` l == 0) combinations
  in foldl (\_ (h,l) -> h `div` l) 0 pairs
      

main :: IO ()
main = do
  inputData <- readFile "./input2"
  let result = countChecksum countDifference' inputData
  print result