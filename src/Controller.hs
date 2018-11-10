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
  | keyStateW gstate == Down = return $ (movePlayer (playerSpeed(player gstate)) gstate){player = rotation (player gstate)} --the world moves respectively to the player                               
       --return gstate
  | otherwise
  = -- Just update the elapsed time
  return $ gstate {asteroids = map move (asteroids gstate), enemies = map (moveEnemy(player gstate)) (enemies gstate), player = rotation (player gstate)} -- no button is pressed, the world moves normally

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) cs _ _) gstate | paused gstate = case cs of Down -> case c of 'p' -> gstate {paused = False}
                                                                                          _ -> gstate
                                                                        _ -> gstate
                                           | otherwise = case cs of Down -> case c of 'w' -> gstate {player = (player gstate) {playerSpeed = playerSpeed (player gstate)}, keyStateW = Down} -- move forward and keep track of the keyState of W
                                                                                      'a' -> gstate {player = (player gstate) {playerRotation = 0.5}} --rotate left
                                                                                      'd' -> gstate {player = (player gstate) {playerRotation = -0.5}} --rotate right
                                                                                      'p' -> gstate {paused = True}
                                                                                      'n' -> case gameOver gstate of True -> initialState (genny gstate)
                                                                                                                     _ -> gstate
                                                                                      _ -> gstate                                             
                                                                    _ -> case c of 'w' -> (movePlayer (0,0) gstate) {keyStateW = Up}
                                                                                   _ -> gstate -- don't change keyStateW to up, because we might press two buttons at a time
inputKey _ gstate = gstate  -- Otherwise keep the same

--The world moves instead of the player
--If the playerSpeed is zero, asteroids will move normally. Otherwise, the playerSpeed is substracted from the Astroid positions 
movePlayer :: Vector -> GameState -> GameState
movePlayer velocity gstate = gstate {asteroids = map (move.moveARespectively) (asteroids gstate), enemies = map(moveEnemy (player gstate).moveERespectively) (enemies gstate)}
    where moveARespectively :: Asteroid -> Asteroid
          moveARespectively ast = ast {position = pointMinuspoint (position ast) velocity}
          moveERespectively :: Enemy -> Enemy
          moveERespectively en = en {enemyPosition = pointMinuspoint (enemyPosition en) velocity}
       