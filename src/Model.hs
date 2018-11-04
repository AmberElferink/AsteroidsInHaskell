-- | This module contains the data types
--   which represent the state of the game

module Model where
import GameObjects
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.01667 --60 FPS

data GameState = GameState {
                   genny :: StdGen,
                   asteroids :: [Asteroid],
                   player :: Player,
                   keyStateW :: KeyState,
                   paused :: Bool,
                   gameOver :: Bool,
                   elapsedTime :: Float                   
                 }

initialState :: StdGen -> GameState
initialState randomgen = GameState lastGenerator initialAsteroidList initialPlayer Up False False 0
  where
    --kleine scherm loopt van (-200, -200) linksonder, naar (200, 200) rechtsboven op vierkantje scherm, bij groot scherm:
    --scherm loopt van (-960, -540) dat is 1920x1080/2linksonder, naar (960, 540) rechtsboven
    initialAsteroidList :: [Asteroid]
    initialAsteroidList = [Asteroid { speed = speed1, position = position1, size = 50},
                           Asteroid { speed = speed2, position = position2, size = 90}]
    (speed1, gen1) = generateTwoNumbers (-9, 9) (-9, 9) randomgen
    (speed2, gen2) = generateTwoNumbers (-9, 9) (-9, 9) gen1
    (position1, gen3) = generateTwoNumbers (-960, 960) (-540, 540) gen2
    (position2, gen4) = generateTwoNumbers (-960, 960) (-540, 540) gen3
    lastGenerator = gen4

initialPlayer :: Player
initialPlayer = Player {playerPosition = [(-25,-25), (0, 50), (25,-25)], lives = 3, playerSpeed = (0,20), rateOfFire = 1, bulletSpeed = 3, playerRotation = 0}



generateTwoNumbers :: RandomGen g => (Float, Float) -> (Float, Float) -> g -> ((Float, Float), g)
generateTwoNumbers interval1 interval2 g = let (v1, g1) = randomR interval1 g
                                               (v2, g2) = randomR interval2 g1
                                            in ((v1, v2), g2)




