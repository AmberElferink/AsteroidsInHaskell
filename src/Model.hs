-- | This module contains the data types
--   which represent the state of the game

module Model where
import GameObjects
import Graphics.Gloss
<<<<<<< HEAD

initialAsteroidList :: [Asteroid]
initialAsteroidList = [Asteroid {speed = (100, 100), position = (-100, -100), size = 50}, Asteroid {speed = (5, 5), position = (50, 50), size = 90}]


=======
import Gameobjects
  
>>>>>>> 7ecd6cf83b6afea7811e3c29588aff2b698d59db
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowAsteroids [Asteroid]

nO_SECS_BETWEEN_CYCLES :: Float
<<<<<<< HEAD
nO_SECS_BETWEEN_CYCLES = 0.01
=======
nO_SECS_BETWEEN_CYCLES = 0.1
>>>>>>> 7ecd6cf83b6afea7811e3c29588aff2b698d59db

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState (ShowAsteroids initialAsteroidList) 0