{-# LANGUAGE MultiParamTypeClasses #-}
module GameObjects where

import Graphics.Gloss


data Asteroid = Asteroid {
  speed :: Point,
  position :: Point,
  size :: Float
}

data Player = Player {
    playerPosition :: Path,
    lives :: Int,
    playerSpeed :: Point,
    rateOfFire :: Float,
    bulletSpeed :: Float,
    playerRotation :: Float
}

class Move a where 
    move :: a -> a

class Collision a b where
    collision :: a -> b -> Bool

pointMinuspoint, pointPlusPoint :: Point -> Point -> Point
pointMinuspoint (x1,y1) (x2,y2) = (x1 - x2,y1 - y2)
pointPlusPoint (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

instance Collision Player Asteroid where
    collision p a = any (< size a) (map (ikbenhetzat.magnitude (position a)) (playerPosition p ++ [(0,0)]))
        where magnitude :: Point -> Point -> Float
              magnitude (ax,ay) (px, py) = sqrt((**)(ax-px) + (**) (ay-py))
              ikbenhetzat :: Float -> Float
              ikbenhetzat i = i - magnitude (speed a) (0,0)
              (**) :: Float -> Float
              (**) x = x * x

class Draw a where
    draw :: a -> Picture

class Rotation a where
    rotation :: Float -> a -> a
   
instance Rotation Player where 
    rotation i a = a {playerPosition = map (rotate' i) (playerPosition a), playerSpeed = rotate' i (playerSpeed a), playerRotation = playerRotation a + i}
       where rotate' :: Float -> Point -> Point
             rotate' r (x,y) = (x * cos r - y * sin r, x * sin r + y * cos r)
     
instance Move Asteroid where
    move a = a {speed = speed a, position = psition (position a) (speed a)}
      where psition :: Point -> Point -> Point
            psition (px, py) (sx, sy) = (px + sx, py + sy) 
    
{- movePlayer :: Player -> Player 
movePlayer a = a {playerSpeed = playerSpeed a, playerPosition = psitionall (playerPosition a) (playerSpeed a)}
      where psitionall :: Path -> Point -> Path
            psitionall pad velocity =  map (psition velocity) pad 
            psition :: Point -> Point -> Point
            psition (px, py) (sx, sy) = (px + sx, py + sy)  -}

instance Draw Asteroid where
    draw a = translate (fst (position a)) (snd (position a)) (color red (circle (size a)))

instance Draw Player where
    draw p = color red (polygon (playerPosition p))
