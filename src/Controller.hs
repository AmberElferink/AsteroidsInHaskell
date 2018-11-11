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
  | any (collision (player gstate)) (asteroids gstate) ||  any (collision (player gstate)) (enemies gstate) = return gstate {gameOver = True}
  | otherwise = do let basicGs = gstate {asteroids = map move (asteroids (astBulRemove gstate)), enemies = map (moveEnemy(player gstate)) (enemies (enBulRemove gstate)), player = rotation (player gstate), bullets = map move (bullets ((astBulRemove.enBulRemove) gstate)), elapsedTime = elapsedTime gstate + secs} 
                   shootEnGS <- case secs + elapsedTime basicGs >= 5 of True -> return basicGs {bullets = bullets basicGs ++ concatMap shoot (enemies gstate), elapsedTime = 0} 
                                                                        _    -> return basicGs {elapsedTime = elapsedTime basicGs + secs} 
                   moveP <- case keyStateW shootEnGS of Down -> return (movePlayer (playerSpeed (player gstate)) shootEnGS)
                                                        _    -> return shootEnGS
                   return ((astBulRemove.enBulRemove) moveP)

astBulRemove :: GameState -> GameState
astBulRemove gstate = gstate { bullets = bMustBeDeleted checkBulAsColls (bullets gstate), asteroids = notBMustBeDeleted checkBulAsColls (asteroids gstate)}
    where checkBulAsColls :: [(Bool, Bullet, Asteroid)]
          checkBulAsColls = [(collision b a, b, a) | b <- bullets gstate, a <- asteroids gstate]

enBulRemove :: GameState -> GameState
enBulRemove gstate = gstate {bullets = bMustBeDeleted checkBulEnColls (bullets gstate), enemies = notBMustBeDeleted checkBulEnColls (enemies gstate)}
    where checkBulEnColls :: [(Bool, Bullet, Enemy)]
          checkBulEnColls = [(collision b e, b, e) | b <- bullets gstate, e <- enemies gstate]

bMustBeDeleted :: [(Bool, Bullet, a)] -> [Bullet] -> [Bullet]
bMustBeDeleted _ [] = []
bMustBeDeleted [] bs = bs
bMustBeDeleted ((True, b, _):xs) bs = bMustBeDeleted xs (deletFromList b bs)
bMustBeDeleted ((_, _, _):xs) bs = bMustBeDeleted xs bs

notBMustBeDeleted :: Eq a => [(Bool, Bullet, a)] -> [a] -> [a]
notBMustBeDeleted _ [] = []
notBMustBeDeleted [] as = as
notBMustBeDeleted ((True, _, a):xs) as = notBMustBeDeleted xs (deletFromList a as)
notBMustBeDeleted ((_, _, _):xs) as = notBMustBeDeleted xs as

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
movePlayer velocity gstate = gstate {asteroids = map (move.moveARespectively) (asteroids gstate), enemies = map(moveEnemy (player gstate).moveERespectively) (enemies gstate), bullets = map(move.moveBRespectively) (bullets gstate)}
    where moveARespectively :: Asteroid -> Asteroid
          moveARespectively ast = ast {position = (-.) (position ast) velocity}
          moveERespectively :: Enemy -> Enemy
          moveERespectively en = en {enemyPosition = (-.) (enemyPosition en) velocity}
          moveBRespectively :: Bullet -> Bullet
          moveBRespectively bul = bul {bPosition = (-.) (bPosition bul) velocity}
       