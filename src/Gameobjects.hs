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
    rotation :: Float,
    rateOfFire :: Float,
    bulletSpeed :: Float
}

class Move a where 
    move :: a -> a

class Draw a where
    draw :: a -> Picture

instance Move Asteroid where
    move a = a {speed = speed a, position = psition (position a) (speed a)}
      where psition :: Point -> Point -> Point
            psition (px, py) (sx, sy) = (px + sx, py + sy) 

instance Move Player where 
    move a = a {playerSpeed = playerSpeed a, playerPosition = psitionall (playerPosition a) (playerSpeed a)}
      where psitionall :: Path -> Point -> Path
            psitionall pad velocity =  map (psition velocity) pad 
            psition :: Point -> Point -> Point
            psition (px, py) (sx, sy) = (px + sx, py + sy) 



instance Draw Asteroid where
    draw a = translate (fst (position a)) (snd (position a)) (color red (circle (size a)))

instance Draw Player where
    draw p = color red (polygon (playerPosition p))


initialAsteroidList :: [Asteroid]
initialAsteroidList = [Asteroid { speed = (6, 1), position = (-100, -100), size = 50}, Asteroid {speed = (5, 5), position = (50, 50), size = 90}]

initialPlayer :: Player
initialPlayer = Player {playerPosition = [(50,50), (75, 100), (100,50)], lives = 3, playerSpeed = (0,6), rotation = 3.4, rateOfFire = 1, bulletSpeed = 3} 
