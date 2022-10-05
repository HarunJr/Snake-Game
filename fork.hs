import Control.Monad
import Control.Concurrent
import System.IO

noBuffering :: IO ()
noBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

data Event = TickEvent | KeyEvent Char deriving Show

tick :: Chan Event -> IO ()
tick chan = forever $ do
  threadDelay (5 * (10 ^ 5))
  writeChan chan TickEvent

input :: Chan Event -> IO ()
input chan = forever $ do
  hSetEcho stdin False
  c <- getChar
  hSetEcho stdin True
  writeChan chan (KeyEvent c)

main = do
  noBuffering
  chan <- newChan
  forkIO $ tick chan
  forkIO $ input chan
  forever $ do
    threadDelay (10 ^ 4)
    c <- readChan chan
    case c of
      TickEvent   -> putStrLn ("hey, i got a tick event!")
      KeyEvent c  -> putStrLn ("hey, you entered " ++ [c])
