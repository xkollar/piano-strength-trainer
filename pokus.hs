{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Arrow ((***))
import Control.Concurrent (threadDelay)
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.List (intercalate)
import System.Environment (getArgs)

import Control.Monad.State

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
    spawnEvent . always $ midiMagic a n

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

--------------

midiMagic :: Line -> Int -> IO ()
midiMagic l n = do
    openInput n >>= \case
        Left s -> runStream pe s
        Right e -> print e
  where
    pe :: PMEvent -> Huu ()
    pe PMEvent{..} = do
        when (status == midiDown) $ do
            lift $ print (timestamp, dec)
            modify $ Map.insert k v
            s <- Map.toList <$> get
            void . lift $ (l # coord (map whoop s))
            pure ()
      where
        whoop = fff *** fff
        fff x = fromIntegral $ x * 2 + 10
        dec@PMMsg{..} = decodeMsg message

        k = fromIntegral data1

        v = fromIntegral data2

--------------

type Huu = StateT (Map.Map Int Int) IO

runHuu :: Huu a -> IO a
runHuu a = evalStateT a Map.empty

main2 = do
    initialize >>= print
    openInput 3 >>= \case
        Left s -> runStream procEvent s
        Right e -> print e
  where
    noDevice = error "no default input device"

runStream f s = runHuu . forever $ do
    lift (readEvents s) >>= \case
        Left evs -> mapM_ f evs
        Right NoError -> pure ()
        Right e -> lift $ print e
    lift $ threadDelay 1000

midiDown = 0x90
midiUp = 0x80

procEvent :: PMEvent -> Huu ()
procEvent PMEvent{..} = do
    when (status == midiDown) $ do
        lift $ print (timestamp, dec)
        modify $ Map.insert k v
  where
    dec@PMMsg{..} = decodeMsg message

    k = fromIntegral data1

    v = fromIntegral data2
