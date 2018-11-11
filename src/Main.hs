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
main = do   eitherEnemy <- eitherDecode <$> B.readFile "Enemies.json" :: IO (Either String [Enemy])
            rng <- getStdGen
            case eitherEnemy of
              Left err -> putStrLn err
              Right initEnemies -> playIO (InWindow "Counter" (1900, 1000) (0, 0)) -- Or FullScreen
                                black            -- Background color
                                100               -- Frames per second
                                (initialState rng initEnemies)
                                view             -- View function
                                input            -- Event function
                                step             -- Step function





