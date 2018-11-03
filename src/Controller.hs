-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import GameObjects

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | Down `elem` keyStates gstate = return $ gstate { player = move (player gstate), asteroids = map move (asteroids gstate)}  
                                         
       --return gstate
  | otherwise
  = -- Just update the elapsed time
  return $ gstate {asteroids = map move (asteroids gstate)}   

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) cs _ _) gstate = case cs of 
                                           Down -> case c of 'w' -> gstate {player = move (player gstate), keyStates = changeElement 0 Down (keyStates gstate)} --changePlayerInGS gstate -- If the user presses a character key, show that one
                                                             'a' -> gstate {player = rotation 1.0 (player gstate)} 
                                                             'd' -> gstate {player = rotation (-1.0) (player gstate)}                                                
                                                             _  -> gstate
                                           _ -> gstate {keyStates = initialKeys }
inputKey _ gstate = gstate  -- Otherwise keep the same

changeElement :: Int -> a -> [a] -> [a]
changeElement _ _ [] = []
changeElement n newElement xs = take n xs ++ [newElement] ++ drop (n + 1) xs      
    