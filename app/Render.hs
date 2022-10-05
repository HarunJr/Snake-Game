{-# LANGUAGE FlexibleContexts #-}

module Render (
  renderOver,
  renderNext,
  renderSnake
) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import System.Console.ANSI

import Data
import Util

renderCharacter :: Char -> Point -> IO ()
renderCharacter c (y, x) = do
  setCursorPosition y x
  putChar c
  return ()

renderSnake :: Char -> Char -> Snake -> IO ()
renderSnake c d (Snake (p : ps)) = do
  renderCharacter c p
  mapM_ (renderCharacter d) ps

renderNext :: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
renderNext = do
  config <- ask
  game <- get
  let (y, x) = screenSize config
  io clearScreen
  io $ renderSnake (headChar config) (bodyChar config) (snake game)
  io $ renderCharacter (foodChar config) (getFood . food $ game)
  io $ setCursorPosition (y + 2) (x - 7)
  io . putStrLn $ "Score " ++ (show . score $ game)

renderOver :: (MonadIO m, MonadReader Config m) => m ()
renderOver = do
  c <- ask
  let (row, col) = screenSize c
  forever $ do
    after (blinkRate c) $ liftIO clearScreen
    after (blinkRate c) $ do
      liftIO $ setCursorPosition (div row 2) (div col 2 - 7)
      liftIO $ putStr "Game Over"
