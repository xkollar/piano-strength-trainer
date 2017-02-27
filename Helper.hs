{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Helper where

import Control.Exception (bracket)
import Control.Arrow ((***))
import Control.Concurrent (threadDelay)
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.List (intercalate)
import Foreign.C.Types (CLong)
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

openInput' :: DeviceID -> IO PMStream
openInput' n = openInput n >>= \case
    Left s -> pure s
    Right e -> error (show e)

withDeviceStream :: DeviceID -> (PMStream -> IO c) -> IO c
withDeviceStream n = bracket (openInput' n) close

withEvents :: MonadIO m => (PMEvent -> m ()) -> PMStream -> m ()
withEvents f s = forever $ do
    liftIO (readEvents s) >>= \case
        Left evs -> mapM_ f evs
        Right NoError -> pure ()
        Right e -> liftIO $ print e
    -- it is OK to check only so often, as eye won't process
    -- in at higher frequency than 100fps anyway...
    liftIO $ threadDelay 10000

midiDown :: CLong
midiDown = 0x90

midiUp :: CLong
midiUp = 0x80

type Huu = StateT (Map.Map Int Int) IO

runHuu :: Huu a -> IO a
runHuu a = evalStateT a Map.empty

exampleProcEvent :: PMEvent -> Huu ()
exampleProcEvent PMEvent{..} =
    when (status == midiDown) $ do
        lift $ print (timestamp, dec)
        modify $ Map.insert k v
  where
    dec@PMMsg{..} = decodeMsg message

    k = fromIntegral data1

    v = fromIntegral data2