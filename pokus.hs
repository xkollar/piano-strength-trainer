{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Exception (bracket)
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

data NumberedDeviceInfo = NumberedDeviceInfo DeviceID DeviceInfo

instance Show NumberedDeviceInfo where
    show (NumberedDeviceInfo i DeviceInfo{..}) =
        show i <> ": " <> name <> " (" <> intercalate "," inf <> ")"
      where
        inf = (interface:) . f output "output" $ f input "input" []
        f b x = if b then (x:) else id

getNumberedDeviceInfo :: DeviceID -> IO NumberedDeviceInfo
getNumberedDeviceInfo i = NumberedDeviceInfo i <$> getDeviceInfo i

listDevices :: IO [NumberedDeviceInfo]
listDevices = do
    c <- countDevices
    sequence $ getNumberedDeviceInfo <$> [0..pred c]

printDevices :: IO ()
printDevices = listDevices >>= mapM_ print

canvasW = 800
canvasH = 600

canvasDim = (canvasW, canvasH)

mainGr :: DeviceID -> IO ()
mainGr n = do
    mainW <- initHTk [text "MIDI Plot"]
    devInfo <- getNumberedDeviceInfo n
    l <- newLabel mainW [text $ show devInfo]
    c <- newCanvas mainW [size canvasDim, background "white"]
    pack l [Expand On]
    pack c [Expand On]
    a <- createLine c
        [ coord []
        , capstyle CapRound
        , joinstyle JoinMiter
        , filling "black"
        , outlinewidth 1
        ]
    koliecko <- createOval c [size (5,5)]
    kt <- spawnEvent . always $ midiMagic a n

    finishHTk
    kt

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

midiMagic :: Line -> DeviceID -> IO ()
midiMagic l n = withDeviceStream n $ withEvents pe
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
        whoop = fixInst *** fixStren
        fixInst n = fromIntegral n * canvasW `div` 127
        fixStren n = (127-fromIntegral n) * canvasH `div` 127
        dec@PMMsg{..} = decodeMsg message

        k = fromIntegral data1

        v = fromIntegral data2

--------------

type Huu = StateT (Map.Map Int Int) IO

runHuu :: Huu a -> IO a
runHuu a = evalStateT a Map.empty

openInput' :: DeviceID -> IO PMStream
openInput' n = openInput n >>= \case
    Left s -> pure s
    Right e -> error (show e)

withDeviceStream :: DeviceID -> (PMStream -> IO c) -> IO c
withDeviceStream n = bracket (openInput' n) close

main3 n = withDeviceStream n . withEvents $ \ ev -> do
    lift $ print ev

withEvents f s = runHuu . forever $ do
    lift (readEvents s) >>= \case
        Left evs -> mapM_ f evs
        Right NoError -> pure ()
        Right e -> lift $ print e
    -- it is OK to check only so often, as eye won't process
    -- in at higher frequency than 100fps anyway...
    lift $ threadDelay 10000

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
