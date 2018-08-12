module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot Bearing Integer Integer

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

bearing :: Robot -> Bearing
bearing (Robot b _ _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ x y) = (x, y)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coord = Robot direction ( fst coord ) ( snd coord )

simulate :: Robot -> String -> Robot
simulate robot instructions = compose ( map go instructions ) robot
  where
    go 'L' = robotLeft
    go 'A' = advance
    go 'R' = robotRight
    go _ = id

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft East  = North
turnLeft South = East
turnLeft West  = South

turnRight :: Bearing -> Bearing
turnRight = turnLeft . turnLeft . turnLeft

advance :: Robot -> Robot
advance (Robot dir x y)
  | dir == North = Robot dir x (succ y)
  | dir == East  = Robot dir (succ x) y
  | dir == South = Robot dir x (pred y)
  | dir == West  = Robot dir (pred x) y
  | otherwise = Robot dir x y

robotRight :: Robot -> Robot
robotRight (Robot dir x y) = Robot (turnRight dir) x y

robotLeft :: Robot -> Robot
robotLeft (Robot dir x y) = Robot (turnLeft dir) x y




