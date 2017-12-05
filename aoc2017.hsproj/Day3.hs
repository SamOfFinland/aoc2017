module Day3 where

import Prelude hiding (Right, Left)

data Direction = Right | Up | Left | Down deriving (Show)
data Point = Point Int Int deriving (Show)

directions = 
  let dirs = cycle [[Right, Up], [Left, Down]]
      multiplyItems n coll= concatMap (replicate n) coll     
  in concat $ zipWith multiplyItems [1..] dirs
  
move direction (Point x y) =
  case direction of
    Right -> Point (succ x) y
    Up    -> Point x (succ y)
    Left  -> Point (pred x) y
    Down  -> Point x (pred y)
    
getCoords steps = foldl (flip move) (Point 0 0) $ take steps directions

stepsToHome afterSteps = abs x + abs y
  where (Point x y) = getCoords $ pred afterSteps


