-- | This module contains the data types
--   which represent the state of the game

module Model where
import GameObjects
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5 --60 FPS

data GameState = GameState {
                   genny :: StdGen,
                   initialEnemies :: [Enemy],
                   asteroids :: [Asteroid],
                   player :: Player,
                   enemies :: [Enemy],
                   bullets :: [Bullet],
                   stars :: [Star],
                   animations :: [Animation],
                   keyStateW :: KeyState,
                   paused :: Bool,
                   gameOver :: Bool,
                   timeStamp :: Float,
                   elapsedTime :: Float                   
                 }

initialState :: StdGen -> [Enemy]-> GameState
initialState randomgen initialEn = GameState lastGenerator initialEn initialAsteroidList initialPlayer initialEn [] initialStarList [explosion (0, 0)] Up False False 0 0
  where
    --kleine scherm loopt van (-200, -200) linksonder, naar (200, 200) rechtsboven op vierkantje scherm, bij groot scherm:
    --scherm loopt van (-960, -540) dat is 1920x1080/2linksonder, naar (960, 540) rechtsboven
    generateAsteroids :: ([Asteroid], StdGen)
    generateAsteroids = generateInitialAsteroids 15 randomgen []
    initialAsteroidList = fst generateAsteroids
    gen2 = snd generateAsteroids
    generateStars = generateInitialStars 5000 gen2 []
    initialStarList = fst generateStars
    lastGenerator = snd generateStars
    initialPlayer :: Player
    initialPlayer = Player {playerPosition = [(-25,-25), (0, 50), (25,-25)], lives = 3, currentPSpeed = (0,0), horsePower = (0,30), bulletSpeed = 50, playerRotation = 0}



generateTwoNumbers :: (Float, Float) -> (Float, Float) -> StdGen -> ((Float, Float), StdGen)
generateTwoNumbers interval1 interval2 g = let (v1, g1) = randomR interval1 g
                                               (v2, g2) = randomR interval2 g1
                                            in ((v1, v2), g2)

data ScreenArea = LeftUpper | RightUpper | RightLower | LeftLower deriving(Enum, Bounded, Eq, Show)

--takes: amount of asteroids, the randomgen, [], and outputs the asteroidslist and the last randomgenerator
generateInitialAsteroids :: Int -> StdGen -> [Asteroid]-> ([Asteroid], StdGen)
generateInitialAsteroids 0 g as = (as, g)
generateInitialAsteroids n g as = generateInitialAsteroids (n - 1) gen4 ((Asteroid { speed = speed1, position = position1, size = size1}):as)
  where (randomPos, gen1) = random g :: (ScreenArea, StdGen)
        (speed1, gen2) = generateTwoNumbers (-20, 20) (-20, 20) gen1
        (position1, gen3) = generatePositions randomPos gen2 
        (size1, gen4) = randomR (10, 70) gen3
        generatePositions :: ScreenArea -> StdGen -> (Point, StdGen) --asteroids spawnt outside of a box of 100 x 100 around the player.
        generatePositions a g4 | a == LeftUpper = generateTwoNumbers (-960, -100) (-100, -540) g4
                               | a == LeftLower = generateTwoNumbers (-960, -100) (540, 100) g4
                               | a == RightLower = generateTwoNumbers (100, 960) (540, 100) g4
                               | a == RightUpper = generateTwoNumbers (100, 960) (-100, -540) g4

generateInitialStars :: Int -> StdGen -> [Star]-> ([Star], StdGen)
generateInitialStars 0 g as = (as, g)
generateInitialStars n g as = generateInitialStars (n - 1) gen4 ((Star { sRelSpeed = speed1, sPosition = position1, sSize = size1}):as)
  where (speed1, gen2) = randomR (0.3, 3) g
        (position1, gen3) = generateTwoNumbers (-5000, 5000) (-5000, 5000) gen2
        (size1, gen4) = randomR (2, 4) gen3



-- inspired and partially copied from: http://sleepomeno.github.io/blog/2017/04/02/Generating-random-Enum-values/
instance Random ScreenArea where
  randomR (a,b) g =
      case randomR (fromEnum a, fromEnum b) g of
              (x, g') -> (toEnum x, g')
  random g = randomR (LeftUpper, LeftLower) g

enumRandomR :: (RandomGen b, Enum a1, Enum a2, Enum a3) => (a1, a2) -> b -> (a3, b)
enumRandomR (a, b) g =
  case randomR (fromEnum a, fromEnum b) g of
    (x, g') -> (toEnum x, g')
    

--this served as inspiration for the star animation: https://stackoverflow.com/questions/19688888/animating-with-gloss-in-haskell
intervalbig :: [Float]
intervalbig = [0,22.5,45,67.5,0,22.5,45,67.5]

explosion :: Point -> Animation
explosion p = Animation { 
  pics = [rotate x (pictures [line [(-8.5,0),(0,50),(8.5,0)], line[(0,50),(0,0)]]) | x <- intervalbig],
  frameN = 0,
  frameMax = 8,
  anPos = p,
  anSpeed = (0, 0),
  amountCycles = 0,
  maxAmountCycles = 15,
  neverEnding = True
}