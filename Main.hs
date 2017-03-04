{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import System.Environment (getArgs)
import Foreign.C.Types (CLong)

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

data MyElement = MyElement
    { age :: Integer
    , element :: Oval
    }

type GrState = StateT (Map.Map CLong MyElement) IO

runMyState :: GrState a -> IO a
runMyState a = evalStateT a Map.empty

ageMod :: (Integer -> Integer) -> MyElement -> MyElement
ageMod f me@MyElement{..} = me{age = f age}

midiMagic :: Canvas -> DeviceID -> IO ()
-- midiMagic c _n = runMyState $ withTestEvents pe'
midiMagic c did = withInputStream did $ runMyState . withEvents pe'
  where
    pe' = withPMMsg pe
    pe :: PMMsg -> GrState ()
    pe PMMsg{..} = when (status == midiDown) $ do
        modify $ Map.mapWithKey af
        gets (Map.lookup k) >>= \case
            Nothing -> do
                o <- liftIO $ createOval c
                    [ size (7,7)
                    , outlinewidth 0
                    , position pos
                    ]
                modify . Map.insert k $ MyElement 0 o
            Just MyElement{..} -> void . liftIO $ do
                void $ element # position pos
                putItemOnTop element

        s <- gets Map.elems
        forM_ s $ \ MyElement{..} -> void . liftIO
            $ element # filling (color age)
      where
        color i = (n, n, n)
          where
            n = fromIntegral $ min (i * 4) 255 :: Int

        af i e = if i == k
            then ageMod (const 0) e
            else ageMod succ e

        pos = (fixInst k, fixStren data2)

        fixInst x = fromIntegral x * canvasW `div` 127

        fixStren x = (127-fromIntegral x) * canvasH `div` 127

        k = data1

mainGr :: DeviceID -> IO ()
mainGr n = do
    b <- isValidDeviceID n
    unless b . error $ "Invalid DeviceID: " <> show n
    mainW <- initHTk [text "MIDI Plot"]
    devInfo <- getNumberedDeviceInfo n
    l <- newLabel mainW [text $ show devInfo]
    c <- newCanvas mainW [size canvasDim, background "white"]
    pack l [Expand Off]
    pack c [Expand On, Fill Both, Anchor Center]

    killMagic <- spawnEvent . always $ midiMagic c n

    finishHTk

    killMagic

main' :: [String] -> IO ()
main' [] = printDevices
main' [n] = mainGr $! read n
main' _ = putStrLn "usage: $PROG [DEVICE_ID]"

main :: IO ()
main = Midi.initialize >> getArgs >>= main'
