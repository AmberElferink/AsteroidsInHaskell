-- | This module defines how to turn
--   the game state into a picture
module View where
import GameObjects
import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [(draw . player) gstate, pictures (map draw (asteroids gstate))] 