import Control.Monad
import Control.Concurrent
import System.IO

main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  forever $ do
    putStrLn ("hey!")
    b <- hWaitForInput stdin (10 ^ 2)
    case b of
      True  -> do
        s <- getChar
        putStrLn ("bye " ++ [s] ++ "!")
      False -> return ()
