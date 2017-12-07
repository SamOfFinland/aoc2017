module Day4 where
  
import Data.List (group, sort)

passphraseValid :: String -> Bool
passphraseValid passphrase = 
  let wordz = words passphrase
      grouped = group $ sort wordz
  in all ((== 1) . length) grouped

main :: IO ()
main = do
  file <- readFile "./passphrases.txt"
  let phrases = lines file
  print $ foldl (\ validPhrases phrase -> 
    if passphraseValid phrase 
    then succ validPhrases 
    else validPhrases) 0 phrases