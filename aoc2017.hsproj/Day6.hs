module Day6 where
  
import qualified Data.Sequence as S (Seq, fromList, length, update, index, foldlWithIndex, adjust)

type Memory = S.Seq Int

clearBankWithMostBlocks :: Memory -> (Memory, Int, Int)
clearBankWithMostBlocks memory =
  let (index, value) = S.foldlWithIndex (\ (i,v) index value -> if value > v then (index,value) else (i,v)) (0,0) memory
      memoryWithClearedBank = S.update index 0 memory
  in (memoryWithClearedBank, index, value)
  
distributeBlocks :: Memory -> Int -> Int -> Memory
distributeBlocks memory blocks index =
  let reallocatedMemory = S.adjust succ index memory
      restBlocks = pred blocks
      nextIndex = nextBank index memory
  in if restBlocks == 0
  then reallocatedMemory
  else distributeBlocks reallocatedMemory restBlocks nextIndex
  
nextBank index memory = mod (succ index) (S.length memory)
  
loopDetected :: [Memory] -> Memory -> Bool
loopDetected dumps memory = any (==memory) dumps

reallocate :: [Memory] -> Memory -> Int
reallocate dumps memory =
  let (clearedBank, index, value) = clearBankWithMostBlocks memory
      newMemory = distributeBlocks clearedBank value $ nextBank index memory
  in 
    if loopDetected dumps newMemory
    then succ $ length dumps
    else reallocate (newMemory:dumps) newMemory