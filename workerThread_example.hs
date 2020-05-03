import Control.Concurrent
import Control.Monad


main :: IO ()
main = do
  m <- newEmptyMVar
  q <- newEmptyMVar
  tryPutMVar m "0"
  
  --
  -- thread to collect input
  --
  forkIO $ forever $ do
    ll <- getLine
    
    when (ll == "1") $ do
      tryPutMVar m "1"
      return ()

    when (ll == "q") $ do
      putMVar q "q"
     
    
  --
  -- thread to work until the input thread tells it not to. 
  -- the input also dictates what it will do
  --
  forkIO $ forever $ do
    nn <- tryTakeMVar m
    when (nn == (Just "1")) $ do
      putStrLn "got it from other thread!"

    putStrLn "working"
    threadDelay 2000000
    

  -- wait for a user to type q
  r <- takeMVar q
  putStrLn "Done"
