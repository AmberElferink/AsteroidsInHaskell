-- | This module contains the data types
--   which represent the state of the game

module Model where
import GameObjects
import Graphics.Gloss
  
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowAsteroids [Asteroid]

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 0.01667 --60 FPS

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState (ShowAsteroids initialAsteroidList) 0