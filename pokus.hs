{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent (threadDelay)
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.List (intercalate)
import System.Environment (getArgs)

import Control.Monad.State (StateT)

import Sound.PortMidi
import HTk.Toplevel.HTk

data NumberedDeviceInfo = NumberedDeviceInfo Int DeviceInfo

instance Show NumberedDeviceInfo where
    show (NumberedDeviceInfo i DeviceInfo{..}) =
        show i <> ": " <> name <> " (" <> intercalate "," inf <> ")"
      where
        inf = (interface:) . f output "output" $ f input "input" []
        f b x = if b then (x:) else id

getNumberedDeviceInfo :: Int -> IO NumberedDeviceInfo
getNumberedDeviceInfo i = NumberedDeviceInfo i <$> getDeviceInfo i

listDevices :: IO [NumberedDeviceInfo]
listDevices = do
    c <- countDevices
    sequence $ getNumberedDeviceInfo <$> [0..pred c]

printDevices :: IO ()
printDevices = listDevices >>= mapM_ print

mainGr :: Int -> IO ()
mainGr n = do
    mainW <- initHTk [text "MIDI Plot"]
    devInfo <- getNumberedDeviceInfo n
    l <- newLabel mainW [text $ show devInfo]
    c <- newCanvas mainW [size (320,240), background "white"]
    pack l [Expand On]
    pack c [Expand On]
    a <- createLine c [coord [], -- [(0,0),(10,10)],
                  capstyle CapRound, joinstyle JoinMiter,
                  filling "black",  outlinewidth 2]
    spawnEvent . always $ magic a 0 []

    finishHTk

magic :: Line -> Integer -> [(Distance,Distance)] -> IO ()
magic l = f
  where
    f n s = do
        l # coord s
        threadDelay 10000
        f (n+1) ((fromIntegral n * 10,  fromIntegral n * 5):s)

main' :: [String] -> IO ()
main' [] = printDevices
main' [n] = mainGr $! read n
main' _ = putStrLn "usage: $PROG [DEVICE_ID]"

main :: IO ()
main = do
    initialize
    getArgs >>= main'

midiDown = 0x90
midiUp = 0x80

type Huu = StateT (Map.Map Int Int) IO

main2 = do
    initialize >>= print
    countDevices >>= print
    c <- countDevices
    mapM_ (\ i -> getDeviceInfo i >>= print) [0..pred c]
    let idid = 3
    openInput idid >>= \case
        Left s -> runStream s
        Right e -> print e
  where
    noDevice = error "no default input device"

runStream s = forever $ do
    readEvents s >>= \case
        Left evs -> mapM_ procEvent evs
        Right NoError -> pure ()
        Right e -> print e
    threadDelay 1000

procEvent PMEvent{..} = do
    print (timestamp, decodeMsg message)
