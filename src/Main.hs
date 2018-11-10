module Main where

import Controller
import Model
import View
import System.Random

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do   rng <- getStdGen
            playIO (InWindow "Counter" (1900, 1000) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              (initialState rng)   -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function