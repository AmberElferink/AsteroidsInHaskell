module Asteroid where

import Graphics.Gloss

data Asteroid = Asteroid {
  speed :: Point,
  position :: Point,
  size :: Float
}

class Move a where 
    move :: a -> a

class Draw a where
    draw :: a -> Picture

instance Move Asteroid where
    move a = a {speed = speed a, position = psition (position a) (speed a)}
      where psition (px, py) (sx, sy) = (px + sx, py + sy) 

instance Draw Asteroid where
    draw a = translate (fst (position a)) (snd (position a)) (color red (circle (size a)))