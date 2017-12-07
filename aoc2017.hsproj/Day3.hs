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
  
type Value = Int
data ValuePoint = ValuePoint Point Value deriving (Show)


calculateValuePoint :: [ValuePoint] -> Point -> ValuePoint
calculateValuePoint valuePoints (Point x y) =
  let deltas = [(dx,dy)| dx <- [-1, 0, 1], dy <- [-1, 0, 1], [dx, dy] /= [0,0]]
      neighbourCoors = map (\(dx,dy) -> (x+dx, y+dy)) deltas
      neighbours = filter (\ (ValuePoint (Point x' y') _) -> any (== (x', y')) neighbourCoors) valuePoints 
      value = foldl (\ sum (ValuePoint _ v) -> sum + v) 0 neighbours
  in ValuePoint (Point x y) value
  
valuePoints = 
  [ ValuePoint (Point 0 0) 1
  , ValuePoint (Point 1 0) 1
  , ValuePoint (Point 1 1) 2
  ]
  
findBiggerValue :: [ValuePoint] -> [Direction] -> Int -> Int
findBiggerValue values directions thresholdValue = 
  let nextDirection = head directions
      ValuePoint currentPoint _ = last values
      nextPoint = move nextDirection currentPoint
      newValuePoint@(ValuePoint _ value) = calculateValuePoint values nextPoint
  in
    if value > thresholdValue
    then value
    else findBiggerValue (values ++ [newValuePoint]) (tail directions) thresholdValue

part2 :: Int -> Int
part2 = findBiggerValue [ValuePoint (Point 0 0) 1] directions

