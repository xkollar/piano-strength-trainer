{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Helper where

import Prelude (error, fromIntegral, pred)

import Control.Applicative (pure)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Function (($), (.), id)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (intercalate)
import Data.Maybe ()
import Data.Monoid
import Foreign.C.Types (CLong)
import System.IO (IO, print)
import System.Random (randomRIO)
import Text.Show (Show, show)

import Control.Monad.State
import qualified Data.Map.Strict as Map

import Sound.PortMidi

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

withTestEvents :: MonadIO m => (PMEvent -> m ()) -> m ()
withTestEvents f = forever $ do
    d1 <- liftIO $ randomRIO (1, 127)
    d2 <- liftIO $ randomRIO (1, 127)
    t <- liftIO time
    f PMEvent
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
