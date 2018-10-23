-- | This module contains the data types
--   which represent the state of the game

module Model where
import GameObjects
import Graphics.Gloss

initialAsteroidList :: [Asteroid]
initialAsteroidList = [Asteroid {speed = (100, 100), position = (-100, -100), size = 50}, Asteroid {speed = (5, 5), position = (50, 50), size = 90}]


data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowAsteroids [Asteroid]

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.01

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0