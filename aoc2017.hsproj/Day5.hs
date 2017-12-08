module Day5 where

import Text.Read (readMaybe)

import qualified Data.Sequence as S (Seq, fromList, length, update, index)

jump :: Int -> Int -> S.Seq Int -> Int
jump index jumps numbers = 
  if not (0 <= index && index < S.length numbers)
  then jumps
  else
    let offset = S.index numbers index
        newOffset = if offset >= 3 then pred offset else succ offset
        newNumbers = S.update index newOffset numbers
        nextIndex = index + offset
    in jump nextIndex (succ jumps) newNumbers
    
beginJumping :: [Int] -> Int
beginJumping list = jump 0 0 $ S.fromList list
    
stringToInt :: String -> Maybe Int
stringToInt = readMaybe
    


main :: IO ()
main = do
  file <- readFile "./jumplist.txt"
  let jumplist = sequence $ map stringToInt $ lines file
      result = fmap beginJumping jumplist
  print result