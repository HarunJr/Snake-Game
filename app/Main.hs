{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import System.Console.ANSI
import System.IO
import System.Random

import Data
import Init
import Render
import Util

movePoint :: Direction -> State Point ()
movePoint "U" = do (x, y) <- get; put (x - 1, y)
movePoint "D" = do (x, y) <- get; put (x + 1, y)
movePoint "L" = do (x, y) <- get; put (x, y - 1)
movePoint "R" = do (x, y) <- get; put (x, y + 1)

keys = [('u', "U"), ('l', "L"), ('r', "R"), ('d', "D")]

canEat :: MonadState Game m => m Bool
canEat = do
  g <- get
  return $ (getFood . food) g == (head . getSnake . snake) g

canPlay :: (MonadReader Config m, MonadState Game m) => m Bool
canPlay = do
  c <- ask
  g <- get
  let (mx, my) = screenSize c
  let Snake ((x, y) : _) = snake g
  return . not $ (x < 0) || (x >= mx) || (y < 0) || (y >= my)

isOpposite :: Direction -> Direction -> Bool
isOpposite "U" "D"  = True
isOpposite "L" "R"  = True
isOpposite "R" "L"  = True
isOpposite "D" "U"  = True
isOpposite  _   _   = False

turnSnake :: MonadState Game m => Direction -> m ()
turnSnake d = do
  g <- get
  if not . isOpposite d $ direction g
  then put (g { direction = d })
  else return ()

moveSnake :: MonadState Game m => m ()
moveSnake = do
  g <- get
  let Snake s@(p : _) = snake g
  let d = direction g
  put (g { snake = Snake (execState (movePoint d) p : init s) })

eatFood :: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
eatFood = do
  game <- get
  config <- ask
  let (scol, srow) = screenSize config
  fcol <- liftIO randomIO
  frow <- liftIO randomIO
  let Food oldFood = food game
  let Snake oldSnake = snake game
  let newFood = (mod fcol scol, mod frow srow)
  put (game { snake = Snake (oldFood : oldSnake), food = Food newFood, score = score game + 1 })

castTick :: Chan Event -> Time -> IO ()
castTick chan rate = forever $ do
  after rate $ writeChan chan TickEvent

castKey :: Chan Event -> IO () 
castKey chan = forever $ do
  hSetEcho stdin False
  c <- getChar
  hSetEcho stdin True
  writeChan chan (KeyEvent c)

proceedGame :: (MonadIO m, MonadReader Config m, MonadState Game m) => Event -> m Bool
proceedGame (KeyEvent k) = do
  case lookup k keys of
    Just d    -> do
      turnSnake d
      return True
    Nothing   -> return True
proceedGame TickEvent = do
  play <- canPlay
  eat <- canEat
  case (play, eat) of
    (False, _)    -> return False
    (True, True)  -> do
      eatFood
      moveSnake
      return True
    (True, False) -> do
      moveSnake
      return True

play :: Chan Event -> Time -> ReaderT Config (StateT Game IO) ()
play chan rate = forever $ do
  event <- io $ readChan chan
  again <- proceedGame event
  if again then renderNext else renderOver

-- main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  chan <- newChan
  (config, game) <- initGame
  forkIO $ castTick chan (tickRate config)
  forkIO $ castKey chan
  runStateT (runReaderT (play chan (blinkRate config)) config) game

