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
initialState randomgen = GameState randomgen (ShowAsteroids initialAsteroidList) 0
  where 
    initialAsteroidList :: [Asteroid]
    initialAsteroidList = [Asteroid { speed = (6, 1), position = (-100, -100), size = 50}, Asteroid {speed = (5, 5), position = (50, 50), size = 90}]
    


generateTwoNumbers :: RandomGen g => g -> ((Int, Int), g)
generateTwoNumbers g = let (v1, g1) = random g
                           (v2, g2) = random g1 -- Use new seed
                           in ((v1, v2), g2) -- Return last seed

