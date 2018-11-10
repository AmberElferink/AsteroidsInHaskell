{-# LANGUAGE MultiParamTypeClasses #-}
module GameObjects where

import Graphics.Gloss

(-.), (+.) :: Point -> Point -> Point
(-.) (x1,y1) (x2,y2) = (x1 - x2,y1 - y2)
(+.) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

pointDiv :: Point -> Float -> Point
pointDiv (x, y) n = ((/) x n, (/) y n)

magnitude :: Point -> Point -> Float
magnitude (ax,ay) (px, py) = sqrt((**)(ax-px) + (**) (ay-py))
    where (**) :: Float -> Float
          (**) x = x * x

unitVector :: Vector -> Vector
unitVector (x,y) = (x/magnitude (x,y) (0,0), y/magnitude(x,y) (0,0))

unitTowardsP :: Player -> Enemy -> Vector
unitTowardsP p e = unitVector ((-.) (middlePointTriangle(playerPosition p)) (enemyPosition e))

mult :: Float -> Vector -> Vector
mult i (x,y) = (x*i, y*i)

lineMiddlePoints :: [Point] -> [Point]
lineMiddlePoints [x,y,z] = [pointDiv ((+.) x y) 2, pointDiv ((+.) z y) 2, pointDiv ((+.) x z) 2]
lineMiddlePoints _ = []

middlePointTriangle :: Path -> Point
middlePointTriangle [(x1,y1), (x2,y2), (x3,y3)]= ((/) (x1+x2+x3) 3, (/) (y1+y2+y3) 3)
middlePointTriangle _ = error "je hebt geen driehoek"

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

data Bullet = Bullet {
    bSize :: Float,
    bSpeed :: Vector,
    bPosition :: Point
}
class Shoot a where
    shoot :: a -> [Bullet]

instance Shoot Player where --DIT
    shoot p = [Bullet {bSpeed = bspeed', bSize = 7, bPosition = middlePointTriangle (playerPosition p)}]
         where bspeed' = mult (bulletSpeed p) (unitVector (playerSpeed p)) 

class Move a where 
    move :: a -> a

class Collision a b where
    collision :: a -> b -> Bool

instance Collision Player Asteroid where
    collision p a = any (<= size a + magnitude (speed a) (0,0)) (map (magnitude (position a)) (playerPosition p ++ [(0,0)] ++ lineMiddlePoints (playerPosition p)))
        
instance Collision Player Enemy where
    collision p e = any (<= sizeOfShip e + magnitude (mult (enemySpeedSize e) (unitTowardsP p e)) (0,0)) (map (magnitude (enemyPosition e)) (playerPosition p ++ [(0,0)] ++ lineMiddlePoints (playerPosition p)))
              
instance Collision Bullet Asteroid where 
    collision b a = bSize b + size a >=  magnitude (bPosition b) (position a)
    
instance Eq Bullet where --to be able to delete one from the list
    Bullet size1 speed1 pos1 == Bullet size2 speed2 pos2 = size1 == size2 && speed1 == speed2 && pos1 == pos2

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
    move a = a {position = (+.) (position a) (speed a)}
     

instance Move Bullet where 
    move a = a {bPosition = (+.) (bPosition a) (bSpeed a)}
            
moveEnemy :: Player -> Enemy -> Enemy 
moveEnemy p e = e {enemyPosition = (+.) (enemyPosition e) (mult (enemySpeedSize e) (unitTowardsP p e))}
    
     
instance Draw Player where
    draw p = color red (polygon (playerPosition p))
instance Draw Asteroid where
    draw a = translate (fst (position a)) (snd (position a)) (color white (circle (size a)))
instance Draw Enemy where
    draw e = translate (fst (enemyPosition e)) (snd (enemyPosition e)) (color white (thickCircle (sizeOfShip e) (sizeOfShip e)))
instance Draw Bullet where 
    draw b = translate (fst (bPosition b)) (snd (bPosition b)) (color white (circle (bSize b)))