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
    playerSpeed :: Vector,
    rateOfFire :: Float,
    bulletSpeed :: Float,
    playerRotation :: Float
}

data Enemy = Enemy {
    enemyPosition :: Point,
    enemySpeedSize :: Float,
    eRateOfFire :: Float,
    eBulletSpeed :: Float,
    sizeOfShip :: Float
}

class Move a where 
    move :: a -> a

class Collision a b where
    collision :: a -> b -> Bool

pointMinuspoint, pointPlusPoint :: Point -> Point -> Point
pointMinuspoint (x1,y1) (x2,y2) = (x1 - x2,y1 - y2)
pointPlusPoint (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

pointDiv :: Point -> Float -> Point
pointDiv (x, y) n = ((/) x n, (/) y n)

magnitude :: Point -> Point -> Float
magnitude (ax,ay) (px, py) = sqrt((**)(ax-px) + (**) (ay-py))
    where (**) :: Float -> Float
          (**) x = x * x



instance Collision Player Asteroid where
    collision p a = any (<= size a + magnitude (speed a) (0,0)) (map (magnitude (position a)) (playerPosition p ++ [(0,0)] ++ lineMiddlePoints (playerPosition p)))
        where lineMiddlePoints :: [Point] -> [Point]
              lineMiddlePoints [] = []
              lineMiddlePoints [x] = []
              lineMiddlePoints [x,y] = []
              lineMiddlePoints [x,y,z] = [pointDiv (pointPlusPoint x y) 2, pointDiv (pointPlusPoint z y) 2, pointDiv (pointPlusPoint x z) 2]
              

class Draw a where
    draw :: a -> Picture

class Rotation a where
    rotation :: a -> a
   
instance Rotation Player where 
    rotation a = a {playerPosition = map (rotate' i) (playerPosition a), playerSpeed = rotate' i (playerSpeed a), playerRotation = 0}
       where rotate' :: Float -> Point -> Point
             rotate' r (x,y) = (x * cos r - y * sin r, x * sin r + y * cos r)
             i = playerRotation a
     
instance Move Asteroid where
    move a = a {position = psition (position a) (speed a)}
      where psition :: Point -> Point -> Point
            psition (px, py) (sx, sy) = (px + sx, py + sy) 
            
moveEnemy :: Player -> Enemy -> Enemy 
moveEnemy p e = e {enemyPosition = psition (enemyPosition e) (mult (enemySpeedSize e) unitTowardsP)}
    where psition :: Point -> Vector -> Point
          psition (px, py) (sx, sy) = (px + sx, py + sy) 
          unitVector :: Vector -> Vector
          unitVector (x,y) = (x/magnitude (x,y) (0,0), y/magnitude(x,y) (0,0))
          unitTowardsP :: Vector
          unitTowardsP = unitVector (pointMinuspoint (0,0) (enemyPosition e))
          mult :: Float -> Vector -> Vector
          mult i (x,y) = (x*i, y*i)
     
instance Draw Player where
    draw p = color red (polygon (playerPosition p))
instance Draw Asteroid where
    draw a = translate (fst (position a)) (snd (position a)) (color white (circle (size a)))
instance Draw Enemy where
    draw e = translate (fst (enemyPosition e)) (snd (enemyPosition e)) (color white (thickCircle (sizeOfShip e) (sizeOfShip e)))