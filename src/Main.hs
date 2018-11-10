module Main where

import Controller
import Model
import View
import GameObjects
import System.Random
import Graphics.Gloss.Interface.IO.Game
--for the IO JSON
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.Aeson
import Data.Text



main :: IO ()
main = do   asteroids <- eitherDecode <$> B.readFile "Asteroids.json" :: IO (Either String [Asteroid])
            case asteroids of
                Left err -> putStrLn err
                Right ps -> print ps
            rng <- getStdGen
            playIO (InWindow "Counter" (1900, 1000) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              (initialState rng asteroids)   -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function