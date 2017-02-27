{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Arrow ((***))
import qualified Data.Map.Strict as Map
import Data.Monoid ()
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

midiMagic :: Line -> DeviceID -> IO ()
midiMagic l _n = runHuu $ withTestEvents pe'
-- midiMagic l n = withDeviceStream n $ runHuu . withEvents pe'
  where
    pe' = withPMMsg pe
    pe :: PMMsg -> Huu ()
    pe ev@PMMsg{..} = when (status == midiDown) $ do
        lift $ print ev
        modify $ Map.insert k v
        s <- gets Map.toList
        -- As it turns out, we have to pass at least two points...
        when (length s >= 2) $
            void . lift $ l # coord (map whoop s)
      where
        whoop = fixInst *** fixStren

        fixInst x = fromIntegral x * canvasW `div` 127

        fixStren x = (127-fromIntegral x) * canvasH `div` 127

        k = fromIntegral data1

        v = fromIntegral data2

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

    finishHTk

    killMagic

main' :: [String] -> IO ()
main' [] = printDevices
main' [n] = mainGr $! read n
main' _ = putStrLn "usage: $PROG [DEVICE_ID]"

main :: IO ()
main = Midi.initialize >> getArgs >>= main'
