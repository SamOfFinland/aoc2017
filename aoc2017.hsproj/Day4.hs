module Day4 where
  
import Data.List (group, sort)

allUnique = all ((== 1) . length)

passphraseValid :: (String -> String) -> String -> Bool
passphraseValid mappingFn passphrase = 
  let wordz = words passphrase
      grouped = group $ sort $ map mappingFn wordz
  in all ((== 1) . length) grouped

main :: IO ()
main = do
  file <- readFile "./passphrases.txt"
  let phrases = lines file
  print $ foldl (\ validPhrases phrase -> 
    -- part 1 if passphraseValid id phrase 
    -- part 2
    if passphraseValid sort phrase
    then succ validPhrases 
    else validPhrases) 0 phrases