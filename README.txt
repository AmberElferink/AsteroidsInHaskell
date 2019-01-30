
Install haskell

open a terminal (for instance cmd) in the main folder.
To set-up the environment, type:
cabal sandbox init
cabal update
cabal install gloss
cabal repl

then install other dependencies it needs. It gives errors on those in the command line.
An example is aeson. Install it like:
cabal install aeson
Do this for all missing dependencies.

Type: cabal run
The game will start.

Move forward with W, and steer with each press of A and D. Shoot with V.
You can Pause the game with P.
You can start a new game with N.
