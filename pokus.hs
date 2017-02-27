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
midiMagic l n = withDeviceStream n $ withEvents pe
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

withEvents :: (PMEvent -> Huu b) -> PMStream -> IO a
withEvents f s = runHuu . forever $ do
    liftIO (readEvents s) >>= \case
        Left evs -> mapM_ f evs
        Right NoError -> pure ()
        Right e -> liftIO $ print e
    -- it is OK to check only so often, as eye won't process
    -- in at higher frequency than 100fps anyway...
    liftIO $ threadDelay 10000

midiDown = 0x90
midiUp = 0x80

exampleProcEvent :: PMEvent -> Huu ()
exampleProcEvent PMEvent{..} =
    when (status == midiDown) $ do
        lift $ print (timestamp, dec)
        modify $ Map.insert k v
  where
    dec@PMMsg{..} = decodeMsg message

    k = fromIntegral data1

    v = fromIntegral data2
