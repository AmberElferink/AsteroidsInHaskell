-- | This module contains the data types
--   which represent the state of the game

module Model where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import GameObjects

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.1

data GameState = GameState {
                   asteroids :: [Asteroid]
                 , player :: Player
                 , keyStates :: [KeyState]
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState initialAsteroidList initialPlayer [Up] 0

initialKeys :: [KeyState]
initialKeys = [Up]