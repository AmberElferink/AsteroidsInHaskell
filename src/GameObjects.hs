{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module GameObjects where
import Graphics.Gloss
import GHC.Generics
import Data.Aeson

secsBetwCycles :: Float
secsBetwCycles = 0.1667 --60 FPS

(-.), (+.) :: Point -> Point -> Point 
(-.) (x1,y1) (x2,y2) = (x1 - x2 * secsBetwCycles, y1 - y2 * secsBetwCycles) --substract two poins
(+.) (x1,y1) (x2,y2) = (x1 + x2 * secsBetwCycles, y1 + y2 * secsBetwCycles) --add two points

pointDiv :: Point -> Float -> Point     -- div a point by a scalar
pointDiv (x, y) n = ((/) x n, (/) y n)

magnitude :: Point -> Point -> Float -- length of a vector between two points
magnitude (ax,ay) (px, py) = sqrt(pow2(ax-px) + pow2 (ay-py))
    where pow2 :: Float -> Float
          pow2 x = x * x

unitVector :: Vector -> Vector
unitVector (x,y) = (x/magnitude (x,y) (0,0), y/magnitude(x,y) (0,0))

unitTowardsP :: Player -> Enemy -> Vector --unit vector in the direction of the player (from enemy)
unitTowardsP p e = unitVector ((-.) (middlePointTriangle(playerPosition p)) (enemyPosition e))

mult :: Float -> Vector -> Vector -- multiply a vector by a scalar
mult i (x,y) = (x*i, y*i)

lineMiddlePoints :: [Point] -> [Point] --calculate the middle points of lines in a triangle
lineMiddlePoints [x,y,z] = [pointDiv ((+.) x y) 2, pointDiv ((+.) z y) 2, pointDiv ((+.) x z) 2]
lineMiddlePoints _ = []

middlePointTriangle :: Path -> Point --calculate the centroid of a triangle
middlePointTriangle [(x1,y1), (x2,y2), (x3,y3)]= ((/) (x1+x2+x3) 3, (/) (y1+y2+y3) 3)
middlePointTriangle _ = error "no triangle input"

data Asteroid = Asteroid {
  speed :: Vector,
  position :: Point,
  size :: Float
} 

data Animation = Animation {
    pics :: [Picture],
    frameN :: Int,
    frameMax :: Int,
    anPos :: Point,
    anSpeed :: Point,
    amountCycles :: Int,
    maxAmountCycles :: Int,
    neverEnding :: Bool
} deriving(Eq)


data Player = Player {
    playerPosition :: Path,
    lives :: Int,
    currentPSpeed :: Vector, --currentSpeed can be zero
    horsePower :: Vector, --speed and direction are stored
    bulletSpeed :: Float,
    playerRotation :: Float
}

data Enemy = Enemy {
    enemyPosition :: Point,
    enemySpeedSize :: Float,
    enemySpeedVec :: Vector, 
    eBulletSpeed :: Float,
    sizeOfShip :: Float
} deriving(Show, Generic)


data Star = Star {
    sRelSpeed :: Float,
    sPosition :: Point,
    sSize :: Float
}

instance ToJSON Enemy where
    toEncoding = genericToEncoding defaultOptions
  
instance FromJSON Enemy 

data Bullet = Bullet {
    bSize :: Float,
    bSpeed :: Vector,
    bPosition :: Point
}
class Shoot a where
    shoot :: a -> [Bullet]

instance Shoot Player where 
    shoot p = [Bullet {bSpeed = bSpeed', bSize = 7, bPosition = mult (40 + bulletSpeed p) (unitVector (horsePower p)) +. frontPlayer (playerPosition p)}]
         where bSpeed' = mult (bulletSpeed p) (unitVector (horsePower p)) +. horsePower p -- bullet speed in direction of the direction the player is looking
               frontPlayer :: Path -> Point --takes the point on the triangle in that is always in front when moving
               frontPlayer [_, y, _] = y
               frontPlayer _ = error "Het is geen driehoek"

instance Shoot Enemy where
    shoot e = [Bullet {bSpeed = bSpeed', bSize = 7, bPosition = enemyPosition e +. mult (2*7 + 15*sizeOfShip e + eBulletSpeed e + enemySpeedSize e) (unitVector bSpeed')}] -- +. enemyPosition e +. mult (sizeOfShip e) (unitVector bSpeed')
         where bSpeed' = mult (eBulletSpeed e + enemySpeedSize e) (unitVector (enemySpeedVec e)) -- bullet speed in direction of the direction the enemy is going

class Move a where 
    move :: a -> a

class Collision a b where
    collision :: a -> b -> Bool

instance Collision Player Asteroid where --check size + magnitude asteroid speed <= any magnitudes asteroid and several points on triangle
    collision p a = any (<= size a + magnitude (speed a) (0,0)) (map (magnitude (position a)) (playerPosition p ++ [(0,0)] ++ lineMiddlePoints (playerPosition p)))
        
instance Collision Player Enemy where --check size + magnitude enemy speed <= any magnitudes enemy and several points on the triangle
    collision p e = any (<= sizeOfShip e + magnitude (mult (enemySpeedSize e) (unitTowardsP p e)) (0,0)) (map (magnitude (enemyPosition e)) (playerPosition p ++ [(0,0)] ++ lineMiddlePoints (playerPosition p)))
              
instance Collision Bullet Asteroid where 
    collision b a = bSize b + size a >=  magnitude (bPosition b) (position a)

instance Collision Bullet Enemy where 
    collision b e = bSize b + 2*sizeOfShip e >=  magnitude (bPosition b) (enemyPosition e) --2*sizeShip because its a thickCircle

instance Collision Player Bullet where --check size b <= any magnitudes enemy and several points on the triangle
    collision p b = any (<= bSize b) (map (magnitude (bPosition b)) (playerPosition p ++ [(0,0)] ++ lineMiddlePoints (playerPosition p)))

instance Eq Bullet where --to be able to delete one from the list
    Bullet size1 speed1 pos1 == Bullet size2 speed2 pos2 = size1 == size2 && speed1 == speed2 && pos1 == pos2

instance Eq Enemy where --to be able to delete one from the list
    Enemy pos1 speed1 speedvec1 bulSpeed1 size1 == Enemy pos2 speed2 speedvec2 bulSpeed2 size2 = pos1 == pos2 && speed1 == speed2 && speedvec1 == speedvec2 && bulSpeed1 == bulSpeed2 && size1 == size2

instance Eq Asteroid where --to be able to delete one from the list
    Asteroid speed1 pos1 size1 == Asteroid speed2 pos2 size2 = speed1 == speed2 && pos1 == pos2 && size1 == size2
    
class Draw a where
    draw :: a -> Picture

class Rotation a where
    rotation :: a -> a
   
instance Rotation Player where --rotate playerPosition, currentSpeed, HorsePower and set playerRation to 0
    rotation a = a {playerPosition = map (rotate' i) (playerPosition a), currentPSpeed = rotate' i (currentPSpeed a), horsePower = rotate' i (horsePower a), playerRotation = 0}
       where rotate' :: Float -> Point -> Point
             rotate' r (x,y) = (x * cos r - y * sin r, x * sin r + y * cos r)
             i = playerRotation a
     
instance Move Asteroid where
    move a = a {position = (+.) (position a) (speed a)}
    
instance Move Bullet where 
    move a = a {bPosition = (+.) (bPosition a) (bSpeed a)}

instance Move Animation where
    move a  | (frameN a + 1) >= frameMax a =  a {frameN = 0, anPos = (+.) (anPos a) (anSpeed a)}
            | otherwise = a {frameN = frameN a + 1, anPos = (+.) (anPos a) (anSpeed a), amountCycles = amountCycles a + 1}
            
moveEnemy :: Player -> Enemy -> Enemy --adjust enemy speed to player position + move
moveEnemy p e = e {enemyPosition = (+.) (enemyPosition e) speedVec', enemySpeedVec = speedVec'}
     where speedVec' :: Vector
           speedVec' = mult (enemySpeedSize e) (unitTowardsP p e)

instance Draw Player where
    draw p = color red (polygon (playerPosition p))
instance Draw Asteroid where
    draw a = translate (fst (position a)) (snd (position a)) (color red (circle (size a)))
instance Draw Enemy where
    draw e = translate (fst (enemyPosition e)) (snd (enemyPosition e)) (color white (thickCircle (sizeOfShip e) (sizeOfShip e)))
instance Draw Bullet where 
    draw b = translate (fst (bPosition b)) (snd (bPosition b)) (color orange (circle (bSize b)))

instance Draw Star where 
    draw s = translate (fst (sPosition s)) (snd (sPosition s)) (color white (circle (sSize s)))

instance Draw Animation where
    draw Animation {pics = as, frameN = fN, frameMax = fM, anPos = anP, anSpeed = _} 
        | fN > fM = blank
        | otherwise = color orange (translate (fst anP) (snd anP) (as !! fN))

