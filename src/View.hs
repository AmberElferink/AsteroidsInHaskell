-- | This module defines how to turn
--   the game state into a picture
module View where
<<<<<<< HEAD

import GameObjects
=======
import Gameobjects
>>>>>>> 7ecd6cf83b6afea7811e3c29588aff2b698d59db
import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
<<<<<<< HEAD
  ShowAsteroids as -> pictures (map draw as)
=======
  ShowAsteroids a -> pictures (map draw a)
>>>>>>> 7ecd6cf83b6afea7811e3c29588aff2b698d59db
