-- | This module contains the data types
--   which represent the state of the game

module Model where
import Graphics.Gloss
  
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
                | ShowCircles [Picture]

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 }

initialState :: GameState
initialState = GameState ShowNothing 0