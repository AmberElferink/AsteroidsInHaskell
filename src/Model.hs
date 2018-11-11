-- | This module contains the data types
--   which represent the state of the game

module Model where
import GameObjects
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

data GameState = GameState {
                   genny :: StdGen,
                   initialEnemies :: [Enemy],
                   asteroids :: [Asteroid],
                   player :: Player,
                   enemies :: [Enemy],
                   bullets :: [Bullet],
                   keyStateW :: KeyState,
                   paused :: Bool,
                   gameOver :: Bool,
                   timeStamp :: Float,
                   elapsedTime :: Float                   
                 }

initialState :: StdGen -> [Enemy]-> GameState
initialState randomgen initialEnemies = GameState lastGenerator initialEnemies initialAsteroidList initialPlayer initialEnemyList [] Up False False 0 0
  where
    --kleine scherm loopt van (-200, -200) linksonder, naar (200, 200) rechtsboven op vierkantje scherm, bij groot scherm:
    --scherm loopt van (-960, -540) dat is 1920x1080/2linksonder, naar (960, 540) rechtsboven
    generateAsteroids :: ([Asteroid], StdGen)
    generateAsteroids = generateInitialAsteroids 10 randomgen []
    initialAsteroidList = fst generateAsteroids
    lastGenerator = snd generateAsteroids
    initialPlayer :: Player
    initialPlayer = Player {playerPosition = [(-25,-25), (0, 50), (25,-25)], lives = 3, playerSpeed = (0,30), rateOfFire = 1, bulletSpeed = 50, playerRotation = 0}



generateTwoNumbers :: (Float, Float) -> (Float, Float) -> StdGen -> ((Float, Float), StdGen)
generateTwoNumbers interval1 interval2 g = let (v1, g1) = randomR interval1 g
                                               (v2, g2) = randomR interval2 g1
                                            in ((v1, v2), g2)

data ScreenArea = LeftUpper | RightUpper | RightLower | LeftLower deriving(Enum, Bounded, Eq, Show)

--takes: amount of asteroids, the randomgen, [], and outputs the asteroidslist and the last randomgenerator
generateInitialAsteroids :: Int -> StdGen -> [Asteroid]-> ([Asteroid], StdGen)
generateInitialAsteroids 0 g as = (as, g)
generateInitialAsteroids n g as = generateInitialAsteroids (n - 1) gen3  ((Asteroid { speed = speed1, position = position1, size = 50}):as)
  where (randomPos, gen1) = random g :: (ScreenArea, StdGen)
        (speed1, gen2) = generateTwoNumbers (-20, 20) (-20, 20) gen1
        (position1, gen3) = generatePositions randomPos gen2 
        generatePositions :: ScreenArea -> StdGen -> (Point, StdGen)
        generatePositions a g4 | a == LeftUpper = generateTwoNumbers (-960, -50) (-540, -490) g4
                               | a == LeftLower = generateTwoNumbers (-960, -50) (540, 50) g4
                               | a == RightLower = generateTwoNumbers (50, 960) (540, 50) g4
                               | a == RightUpper = generateTwoNumbers (50, 960) (-540, -490) g4




-- inspired and partially copied from: http://sleepomeno.github.io/blog/2017/04/02/Generating-random-Enum-values/
instance Random ScreenArea where
  randomR (a,b) g =
      case randomR (fromEnum a, fromEnum b) g of
              (x, g') -> (toEnum x, g')
  random g = randomR (LeftUpper, LeftLower) g


enumRandomR (a, b) g =
  case randomR (fromEnum a, fromEnum b) g of
    (x, g') -> (toEnum x, g')