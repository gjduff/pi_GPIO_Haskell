import qualified Control.Concurrent as C
import qualified Control.Monad as M

gpioPath="/sys/class/gpio/"

red :: String
red = "25"

yellow :: String
yellow = "24"

green :: String
green = "23"

leftSignal :: String
leftSignal = "21"

sensor :: String
sensor = "14"



--
--  enable GPIO pin by writing the string of that pin number to "export" file, 
--  gpioXX directory will then appear. 
--  Set that pin as output by writing "out\n" 
--  Set that pin as intput by writing "in\n" 
--  to "direction" file that is present in any gpioXX directory (XX is the pin number)
--
setUpPin :: String -> String -> IO ()
setUpPin pin inOut = do
  writeFile (gpioPath ++ "export") (pin ++ "\n")
  C.threadDelay 100000
  writeFile (gpioPath ++ "gpio" ++ pin ++ "/direction") (inOut ++ "\n")
  C.threadDelay 100000


--
-- set up all IO pins we will use in this program
--
setUp :: IO ()
setUp = do
  setUpPin red "out"
  setUpPin green "out"
  setUpPin yellow "out"
  setUpPin leftSignal "out"
  setUpPin sensor "in"


--
--  disable GPIO pin by writing the string of that pin number to "unexport" file
--
cleanUpPin :: String -> IO ()
cleanUpPin pin = do
  writeFile (gpioPath ++ "unexport") (pin ++ "\n")
  C.threadDelay 100000


--
-- clean up all IO pins used in this program
--
cleanUp :: IO ()
cleanUp = do
  cleanUpPin red
  cleanUpPin green
  cleanUpPin yellow
  cleanUpPin leftSignal
  cleanUpPin sensor



--
--  turn on pin by writing "1\n"   turn off pin by writing "0\n"
--  to "value" file that is present in any gpioXX directory (XX is the pin number)
--  then delay for the specified time
--
changePin :: String -> String -> Int -> IO ()
changePin pin onOff delay = do
  writeFile (gpioPath ++ "gpio" ++ pin ++ "/value")  (onOff ++ "\n")
  C.threadDelay delay


--
-- turn pin on then off with delay after each operation
--
blinkOn :: String -> Int -> IO ()
blinkOn pin delay = do
  changePin pin  "1" delay
  changePin pin  "0" delay


--
-- read the state of the GPIO input - return
-- "value" file in any gpioXX directory will 
--  be 1 when pin is high, 0 when pin is low
--
readSensor :: String -> IO String
readSensor pin = do
  str <- readFile (gpioPath ++ "gpio" ++ pin ++ "/value")
  return str

--
--  set up one thread for reading the sensor (/button)
--  and another for running the traffic light
--
doIO :: IO ()
doIO = do

  -- for inter-thread communication
  msgSensed <- C.newEmptyMVar
  msgQuit <- C.newEmptyMVar
  C.tryPutMVar msgSensed "0"
  

  -- sensor read thread
  tId1 <- C.forkIO $ M.forever $ do
    str <- readSensor sensor
    M.when (str == "1\n") $ do
      C.tryPutMVar msgSensed "1"
      return ()   
    

  -- getLine/Quit thread
  C.forkIO $ M.forever $ do
    ll <- getLine
    M.when (ll == "q") $ do
      C.putMVar msgQuit "q"  
  
  
  -- worker/sequence thread
  tId2 <- C.forkIO $ M.forever $ do
    nn <- C.tryTakeMVar msgSensed
    
    if (nn == (Just "1")) then do
      changePin green "1"  500000
      mapM_ (blinkOn leftSignal) (replicate 7 500000)
    else
      changePin green "1"  4500000
      
    changePin green  "0"   0
    changePin yellow "1"   2000000
    changePin yellow "0"   0
    changePin red    "1"   4500000
    changePin red    "0"   0
    

  -- wait for a user to type q
  r <- C.takeMVar msgQuit
  
  -- kill worker and sensor thread then cleanup GPIO pins
  C.killThread tId1
  C.killThread tId2
  cleanUp
  putStrLn "Done. GPIO pims cleaned up."


--
--  main program - setup output/input pins then start the loop
--
main :: IO ()
main = do 
  setUp
  doIO 

