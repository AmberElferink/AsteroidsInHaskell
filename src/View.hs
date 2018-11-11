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
                | otherwise = pictures [(draw . player) gstate, pictures (map draw (asteroids gstate)), 
                                        pictures (map draw (enemies gstate)), 
                                        pictures (map draw (bullets gstate)), 
                                        pictures (map draw (animations gstate)),
                                        pictures (map draw (stars gstate))]  




{-viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  ShowAsteroids as -> pictures (map draw as)
-}