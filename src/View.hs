-- | This module defines how to turn
--   the game state into a picture
module View where
import GameObjects
import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate | gameOver gstate = translate (-500) 0 (color white (text "Game Over"))
                | paused gstate = translate (-500) 0 (color white (text "PAUSED"))
                | otherwise = pictures [pictures (map draw (asteroids gstate)), 
                                        pictures (map draw (enemies gstate)), 
                                        pictures (map draw (bullets gstate)), 
                                        pictures (map draw (animations gstate)),
                                        pictures (map draw (stars gstate)),
                                        (draw . player) gstate]  
