module Init (initConfig, initGame) where

import Control.Monad.IO.Class
import Data.Maybe
import System.Console.ANSI
import System.Random

import Data
import Util

-- `MonadIO` is a typeclass which is compatible with every monad stack where IO occurs
initConfig :: MonadIO m => m Config
initConfig = do
    s <- io getTerminalSize
    return $ Config {
        screenSize = fromJust s,
        headChar   = 'o',
        bodyChar   = 'x',
        foodChar   = '&',
        tickRate   = 200,
        blinkRate  = 500
        }

initGame :: MonadIO m => m (Config, Game)
initGame = do
  fcol <- io randomIO
  frow <- io randomIO
  io $ print (fcol, frow)
  config <- initConfig
  let (scol, srow) = screenSize config
  let food = (mod fcol scol, mod frow srow)
  io $ print food
  let game = Game {
    snake      = Snake . reverse $ zipWith ($) (repeat ((,) 10)) [ 10 .. 12 ],
    food       = Food food,
    direction  = "R",
    score      = 0
  }
  return $ (config, game)