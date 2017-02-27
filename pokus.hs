{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Arrow ((***))
import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Map
import Data.Monoid ()
import System.Environment (getArgs)

import Control.Monad.State
import System.Random

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
        void $ createOval c
            [ position (fromIntegral n * 4, fromIntegral n * 4)
            , size (5,5)
            ]
        threadDelay 100000
        f (succ n)

withTestEvents :: MonadIO m => (PMEvent -> m ()) -> m ()
withTestEvents f = forever $ do
    d1 <- liftIO $ randomRIO (1, 127)
    d2 <- liftIO $ randomRIO (1, 127)
    t <- liftIO $ time
    f $ PMEvent
        { timestamp = t
        , message = encodeMsg PMMsg
            { status = midiDown
            , data1 = d1
            , data2 = d2
            }
        }
    liftIO $ threadDelay 100000

withPMMsg :: (PMMsg -> a) -> PMEvent -> a
withPMMsg f PMEvent{..} = f $ decodeMsg message

midiMagic :: Line -> DeviceID -> IO ()
midiMagic l n = withDeviceStream n $ runHuu . const (withTestEvents pe)
-- midiMagic l n = withDeviceStream n $ runHuu . withEvents pe
  where
    pe :: PMEvent -> Huu ()
    pe PMEvent{..} = when (status == midiDown) $ do
        lift $ print (timestamp, dec)
        modify $ Map.insert k v
        s <- Map.toList <$> get
        -- As it turns out, we have to pass at least two points...
        when (length s >= 2) $
            void . lift $ l # coord (map whoop s)
      where
        whoop = fixInst *** fixStren

        fixInst x = fromIntegral x * canvasW `div` 127

        fixStren x = (127-fromIntegral x) * canvasH `div` 127

        dec@PMMsg{..} = decodeMsg message

        k = fromIntegral data1

        v = fromIntegral data2
