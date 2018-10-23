module GameObjects where

import Graphics.Gloss

data Asteroid = Asteroid {
  speed :: Point,
  position :: Point,
  size :: Float
}

class Move a where 
    move :: Float -> a -> a
    moveAll :: Float -> [a] -> [a]

class Draw a where
    draw :: a -> Picture

instance Move Asteroid where
    move s a = a {speed = speed a, position = psition (position a) (speed a)} 
      where psition (px, py) (sx, sy) = (px + sx * s, py + sy * s) 
    moveAll s a = map (move s) a

instance Draw Asteroid where
    draw a = translate (fst (position a)) (snd (position a)) (color red (circle (size a)))