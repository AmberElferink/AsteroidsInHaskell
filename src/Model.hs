-- | This module contains the data types
--   which represent the state of the game

module Model where
import GameObjects
import Graphics.Gloss
import System.Random


data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowAsteroids [Asteroid]


nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.01

data GameState = GameState {
                   genny :: StdGen,
                   infoToShow  :: InfoToShow,
                   elapsedTime :: Float
                 }

initialState :: StdGen -> GameState
initialState randomgen = GameState lastGenerator (ShowAsteroids initialAsteroidList) 0
  where 
    initialAsteroidList :: [Asteroid]
    initialAsteroidList = [Asteroid { speed = speed1, position = (-100, -100), size = 50}, Asteroid {speed = speed2, position = (50, 50), size = 90}]
    (speed1, gen1) = generateTwoNumbers (-9, 9) randomgen
    (speed2, gen2) = generateTwoNumbers (-9, 9) gen1
    lastGenerator = gen2
    


generateTwoNumbers :: RandomGen g => (Float, Float) -> g -> ((Float, Float), g)
generateTwoNumbers interval g = let (v1, g1) = randomR interval g
                                    (v2, g2) = randomR interval g1 -- Use new seed
                                in ((v1, v2), g2) -- Return last seed

