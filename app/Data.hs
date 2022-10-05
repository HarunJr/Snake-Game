module Data (
    Config (..),
    Direction,
    Event (..),
    Food (..),
    Game (..),
    Point,
    Score,
    Size,
    Snake (..),
    Time
  ) where

import Control.Monad.State
import Control.Monad.Reader

data Event
  = TickEvent
  | KeyEvent Char
  deriving Show

type Size = (Int, Int)
type Time = Int
data Config = Config {
    screenSize :: Size,
    headChar   :: Char,
    bodyChar   :: Char,
    foodChar   :: Char,
    tickRate   :: Time,
    blinkRate  :: Time

} deriving Show

type Point = (Int, Int)
newtype Snake = Snake {getSnake :: [Point]} deriving Show
newtype Food = Food {getFood :: Point} deriving Show
type Direction = String
type Score = Int
data Game = Game {
    snake :: Snake,
    food :: Food,
    direction :: Direction,
    score :: Int
  } deriving Show
