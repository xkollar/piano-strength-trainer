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

import Sound.PortMidi hiding (initialize)
import qualified Sound.PortMidi as Midi
import HTk.Toplevel.HTk

import Helper


canvasW :: Distance
canvasW = 800

canvasH :: Distance
canvasH = 600

canvasDim :: Size
canvasDim = (canvasW, canvasH)

mainGr :: DeviceID -> IO ()
mainGr n = do
    mainW <- initHTk [text "MIDI Plot"]
    devInfo <- getNumberedDeviceInfo n
    l <- newLabel mainW [text $ show devInfo]
    c <- newCanvas mainW [size canvasDim, background "white"]
    pack l [Expand Off]
    pack c [Expand On, Fill Both]
    a <- createLine c
        [ coord []
        , capstyle CapRound
        , joinstyle JoinMiter
        , filling "black"
        , outlinewidth 1
        ]
    koliecko <- createOval c [size (5,5)]
    killMagic <- spawnEvent . always $ midiMagic a n
    killExperiment <- spawnEvent . always $ experiment c 1

    finishHTk
    killMagic
    killExperiment

main' :: [String] -> IO ()
main' [] = printDevices
main' [n] = mainGr $! read n
main' _ = putStrLn "usage: $PROG [DEVICE_ID]"

main :: IO ()
main = Midi.initialize >> getArgs >>= main'

--------------

experiment :: Canvas -> Integer -> IO ()
experiment c = f
  where
    f n = do
        createOval c
            [ position (fromIntegral n * 4, fromIntegral n * 4)
            , size (5,5)
            ]
        threadDelay 100000
        f (succ n)

midiMagic :: Line -> DeviceID -> IO ()
midiMagic l n = withDeviceStream n $ runHuu . withEvents pe
  where
    pe :: PMEvent -> Huu ()
    pe PMEvent{..} = when (status == midiDown) $ do
        lift $ print (timestamp, dec)
        modify $ Map.insert k v
        s <- Map.toList <$> get
        void . lift $ l # coord (map whoop s)
      where
        whoop = fixInst *** fixStren

        fixInst n = fromIntegral n * canvasW `div` 127

        fixStren n = (127-fromIntegral n) * canvasH `div` 127

        dec@PMMsg{..} = decodeMsg message

        k = fromIntegral data1

        v = fromIntegral data2
