-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import GameObjects
import Graphics.Gloss.Interface.IO.Game



-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | gameOver gstate = return gstate
  | paused gstate = return gstate
  | any (collision (player gstate)) (asteroids gstate) ||  
    any (collision (player gstate)) (enemies gstate) || 
    any (collision (player gstate)) (bullets gstate) = return gstate {gameOver = True}
  | otherwise = do 
      let basicGs = 
            gstate {
                  asteroids = map move (asteroids (astBulRemove gstate)), 
                  enemies = map (moveEnemy(player gstate)) (enemies (enBulRemove gstate)), 
                  player = rotation (player gstate), 
                  elapsedTime = elapsedTime gstate + secs,
                  animations = map move (deleteDoneAnimations (animations gstate))
                  } 
      let shootEnGS = case secs + elapsedTime basicGs >= 5 of 
            True -> basicGs {bullets = 
                  map move (bullets basicGs ++ concatMap shoot (enemies basicGs)), 
                  elapsedTime = 0} 
            _    -> basicGs {
                  bullets = map move (bullets ((astBulRemove.enBulRemove) basicGs)),  
                  elapsedTime = elapsedTime basicGs + secs} 
      let moveP = case keyStateW shootEnGS of 
                        Down -> movePlayer (playerSpeed (player shootEnGS)) shootEnGS
                        _    -> shootEnGS
      return ((astBulRemove.enBulRemove) moveP)

astBulRemove :: GameState -> GameState
astBulRemove gstate = 
  gstate { bullets = bMustBeDeleted listCollisions (bullets gstate), 
  asteroids = notBMustBeDeleted  listCollisions (asteroids gstate),
  animations = getExplosions listCollisions ++ animations gstate}
    where checkBulAsColls :: GameState -> [(Bool, Bullet, Asteroid)]
          checkBulAsColls gstate = [(collision b a, b, a) | b <- bullets gstate, a <- asteroids gstate]
          listCollisions :: [(Bool, Bullet, Asteroid)] 
          listCollisions = checkBulAsColls gstate




getExplosions lc = map explosion bs
      where fListCollisions = filter (\(b, _, _) -> b) lc
            fBullets :: [Bullet] 
            fBullets = map (\(_, bullet, _) -> bullet) fListCollisions
            bs :: [Point]
            bs = map bPosition fBullets 

enBulRemove :: GameState -> GameState
enBulRemove gstate = gstate {bullets = bMustBeDeleted listCollisions (bullets gstate), 
                             enemies = notBMustBeDeleted listCollisions (enemies gstate),
                             animations = getExplosions listCollisions}
    where checkBulEnColls :: GameState -> [(Bool, Bullet, Enemy)]
          checkBulEnColls gstate = [(collision b e, b, e) | b <- bullets gstate, e <- enemies gstate]
          listCollisions = checkBulEnColls gstate

bMustBeDeleted :: [(Bool, Bullet, a)] -> [Bullet] -> [Bullet]
bMustBeDeleted _ [] = []
bMustBeDeleted [] bs = bs
bMustBeDeleted ((True, b, _):xs) bs = bMustBeDeleted xs (deletFromList b bs)
bMustBeDeleted ((_, _, _):xs) bs = bMustBeDeleted xs bs

notBMustBeDeleted :: Eq a => [(Bool, Bullet, a)] -> [a] -> [a]
notBMustBeDeleted _ [] = []
notBMustBeDeleted [] as = as
notBMustBeDeleted ((True, b, a):xs) as = notBMustBeDeleted xs (deletFromList a as)
notBMustBeDeleted ((_, _, _):xs) as = notBMustBeDeleted xs as

deleteDoneAnimations :: [Animation] -> [Animation]
deleteDoneAnimations [] = []
deleteDoneAnimations (a:as) | amountCycles a >= maxAmountCycles a && neverEnding a == False = deleteDoneAnimations (deletFromList a as)
                            | otherwise = a : deleteDoneAnimations as

deletFromList :: Eq a => a -> [a] -> [a]
deletFromList _ [] = []
deletFromList a (x:xs) | a == x = xs
                       | otherwise = x : deletFromList a xs



-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) cs _ _) gstate | paused gstate = case cs of Down -> case c of 'p' -> gstate {paused = False}
                                                                                          _ -> gstate
                                                                        _ -> gstate
                                           | otherwise = case cs of Down -> case c of 'w' -> gstate {player = (player gstate) {playerSpeed = playerSpeed (player gstate)}, keyStateW = Down} -- move forward and keep track of the keyState of W
                                                                                      'a' -> gstate {player = (player gstate) {playerRotation = 0.5}} --rotate left
                                                                                      'd' -> gstate {player = (player gstate) {playerRotation = -0.5}} --rotate right
                                                                                      'p' -> gstate {paused = True}
                                                                                      'n' -> initialState (genny gstate) (initialEnemies gstate)
                                                                                      'v' -> gstate {bullets = bullets gstate ++ shoot (player gstate)}
                                                                                      _ -> gstate                                             
                                                                    _ -> case c of 'w' -> (movePlayer (0,0) gstate) {keyStateW = Up}
                                                                                   _ -> gstate -- don't change keyStateW to up, because we might press two buttons at a time
inputKey _ gstate = gstate  -- Otherwise keep the same

--The world moves instead of the player
--If the playerSpeed is zero, asteroids will move normally. Otherwise, the playerSpeed is substracted from the Astroid positions 
movePlayer :: Vector -> GameState -> GameState
movePlayer velocity gstate = gstate {
      asteroids = map (move.moveARespectively) (asteroids gstate), 
      enemies = map(moveEnemy (player gstate).moveERespectively) (enemies gstate), 
      bullets = map(move.moveBRespectively) (bullets gstate),
      stars = map moveSRespectively (stars gstate)}
    where moveARespectively :: Asteroid -> Asteroid
          moveARespectively ast = ast {position = (-.) (position ast) velocity}
          moveERespectively :: Enemy -> Enemy
          moveERespectively en = en {enemyPosition = (-.) (enemyPosition en) velocity}
          moveBRespectively :: Bullet -> Bullet
          moveBRespectively bul = bul {bPosition = (-.) (bPosition bul) velocity}
          moveSRespectively :: Star -> Star
          moveSRespectively star = star {sPosition = (-.) (sPosition star) (mult (sRelSpeed star) velocity)}
          
       