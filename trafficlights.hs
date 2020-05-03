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
  setUpPin  sensor "in"


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
--  set up infinite loop for running the traffic light
--
doIO :: IO ()
doIO = do
  str <- readSensor sensor
  putStrLn str
  
  if (str == "1\n") then do
    changePin green "1"  500000
    mapM_ (blinkOn leftSignal) (replicate 7 500000)
  else
    changePin green "1"  4500000
  
  changePin green  "0"   0
  changePin yellow "1"   2000000
  changePin yellow "0"   0
  changePin red    "1"   4500000
  changePin red    "0"   0
  
  doIO


--
--  main program - setup output/input pins then start the loop
--
main :: IO ()
main = do 
  setUp
  doIO 

