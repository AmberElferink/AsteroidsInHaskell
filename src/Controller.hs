-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import GameObjects

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random



-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | gameOver gstate = return gstate
  | paused gstate = return gstate
  | any (collision (player gstate)) (asteroids gstate) = return gstate {gameOver = True}
  | keyStateW gstate == Down = return $ movePlayer (rotatedPlayerSpeed(player gstate)) gstate --the world moves respectively to the player                               
       --return gstate
  | otherwise
  = -- Just update the elapsed time
  return $ gstate {asteroids = map move (asteroids gstate)} -- no button is pressed, the world moves normally

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) cs _ _) gstate = case cs of 
                                           Down -> case c of 'w' -> (movePlayer (rotatedPlayerSpeed(player gstate)) gstate){keyStateW = Down} -- move forward and keep track of the keyState of W
                                                             'a' -> gstate {player = rotation 0.5 (player gstate)} --rotate left
                                                             'd' -> gstate {player = rotation (-0.5) (player gstate)} --rotate right
                                                             'p' -> case paused gstate of True -> gstate {paused = False}
                                                                                          _ -> gstate {paused = True}
                                                             'n' -> case gameOver gstate of True -> initialState (genny gstate)
                                                                                            _ -> gstate                                             
                                                             _  -> gstate
                                           Up -> case c of 'w' -> (movePlayer (0,0) gstate) {keyStateW = Up}
                                                           _ -> gstate -- don't change keyStateW to up, because we might press two buttons at a time
inputKey _ gstate = gstate  -- Otherwise keep the same

--This is the calculation of the rotated playerSpeed when it's not zero
rotatedPlayerSpeed :: Player -> Point
rotatedPlayerSpeed p = pointRotation (playerRotation p) (playerSpeed initialPlayer)
    where pointRotation :: Float -> Point -> Point
          pointRotation r (x,y) = (x * cos r - y * sin r, x * sin r + y * cos r)


--The world moves instead of the player
--If the playerSpeed is zero, asteroids will move normally. Otherwise, the playerSpeed is substracted from the Astroid positions 
movePlayer :: Point -> GameState -> GameState
movePlayer velocity gstate = gstate {player = (player gstate) {playerSpeed = velocity}, asteroids = map (move.moveRespectively) (asteroids gstate) }
    where moveRespectively :: Asteroid -> Asteroid
          moveRespectively ast = ast {position = pointMinuspoint (position ast) velocity}
       