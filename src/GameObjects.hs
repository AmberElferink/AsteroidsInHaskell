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
        bulletSpeed :: Float
    }
    
    class Move a where 
        move :: a -> a
    
    class Draw a where
        draw :: a -> Picture
    
    class Rotation a where
        rotation :: Float -> a -> a
       
    instance Rotation Player where 
        rotation i a = a {playerPosition = map (rotate' i) (playerPosition a), playerSpeed = rotate' i (playerSpeed a)}
           where rotate' :: Float -> Point -> Point
                 rotate' r (x,y) = (x * cos r - y * sin r, x * sin r + y * cos r)
    
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
    
    