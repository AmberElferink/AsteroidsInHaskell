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
    any (collision (player gstate)) (bullets gstate) = return gstate {gameOver = True}   -- if the player collides with asteroid, enemy or bullet -> game over
  | otherwise = do 
      let basicGs = 
            gstate {
                  player = rotation (player gstate),                                     -- rotate player
                  asteroids = asteroids (astBulRemove gstate),
                  enemies = enemies (enBulRemove gstate),
                  bullets = bullets ((astBulRemove.enBulRemove) gstate),
                  elapsedTime = elapsedTime gstate + secs,                               -- update time
                  animations = map move (deleteDoneAnimations (animations gstate))       -- do animations
                  } 
      let shootEnGS = case secs + elapsedTime basicGs >= 5 of                            -- time between enemy shots is checked
            True -> basicGs {bullets = 
                  map move (bullets basicGs ++ concatMap shoot (enemies basicGs)),      -- enemy shoots, bullets move
                  elapsedTime = 0} 
            _    -> basicGs {
                  bullets = map move (bullets ((astBulRemove.enBulRemove) basicGs)),   -- bullets move
                  elapsedTime = elapsedTime basicGs + secs} 
      return (movePlayer shootEnGS)                                                    -- move player and remove enemies/asteroids with collision

astBulRemove :: GameState -> GameState
astBulRemove gstate = 
  gstate { bullets = bMustBeDeleted listCollisions (bullets gstate), 
  asteroids = notBMustBeDeleted  listCollisions (asteroids gstate),
  animations = getExplosions listCollisions ++ animations gstate}
    where checkBulAsColls :: GameState -> [(Bool, Bullet, Asteroid)]
          checkBulAsColls gs = [(collision b a, b, a) | b <- bullets gs, a <- asteroids gs]
          listCollisions :: [(Bool, Bullet, Asteroid)] 
          listCollisions = checkBulAsColls gstate

getExplosions :: [(Bool, Bullet, a)] -> [Animation]
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
          checkBulEnColls gs = [(collision b e, b, e) | b <- bullets gs, e <- enemies gs]
          listCollisions = checkBulEnColls gstate

bMustBeDeleted :: [(Bool, Bullet, a)] -> [Bullet] -> [Bullet]                       -- deletes bullets where collision is true
bMustBeDeleted _ [] = []
bMustBeDeleted [] bs = bs
bMustBeDeleted ((True, b, _):xs) bs = bMustBeDeleted xs (deletFromList b bs)
bMustBeDeleted ((_, _, _):xs) bs = bMustBeDeleted xs bs

notBMustBeDeleted :: Eq a => [(Bool, Bullet, a)] -> [a] -> [a]                      -- deletets other things where collision is true
notBMustBeDeleted _ [] = []
notBMustBeDeleted [] as = as
notBMustBeDeleted ((True, _, a):xs) as = notBMustBeDeleted xs (deletFromList a as)
notBMustBeDeleted ((_, _, _):xs) as = notBMustBeDeleted xs as

deleteDoneAnimations :: [Animation] -> [Animation]
deleteDoneAnimations [] = []
deleteDoneAnimations (a:as) | amountCycles a >= maxAmountCycles a && not(neverEnding a) = deleteDoneAnimations (deletFromList a as)
                            | otherwise = a : deleteDoneAnimations as

deletFromList :: Eq a => a -> [a] -> [a]                                            -- general function for deleting something from a list
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
                                           | otherwise = case cs of Down -> case c of 'w' -> movePlayer (gstate {player = (player gstate) {currentPSpeed = horsePower (player gstate)}, keyStateW = Down}) -- move forward and keep track of the keyState of W
                                                                                      'a' -> movePlayer (gstate {player = (player gstate) {playerRotation = 0.4}}) --rotate left
                                                                                      'd' -> movePlayer (gstate {player = (player gstate) {playerRotation = -0.4}}) --rotate right
                                                                                      'p' -> gstate {paused = True}
                                                                                      'n' -> initialState (genny gstate) (initialEnemies gstate)
                                                                                      'v' -> gstate {bullets = map move (bullets gstate ++ shoot (player gstate))}
                                                                                      _ -> gstate                                             
                                                                    _ -> case c of 'w' -> gstate {player = (player gstate) {currentPSpeed = (0,0)}, keyStateW = Up}
                                                                                   _ -> gstate -- don't change keyStateW to up, because we might press two buttons at a time
inputKey _ gstate = gstate  -- Otherwise keep the same

--The world moves instead of the player
--If the currentPSpeed is zero, asteroids will move normally. Otherwise, the currentPSpeed is substracted from the Astroid positions 
movePlayer :: GameState -> GameState
movePlayer gstate = gstate {
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
          velocity = currentPSpeed (player gstate)
          
       