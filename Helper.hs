{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Helper where

import Prelude (error, fromIntegral, pred)

import Control.Applicative (pure)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.Bool (Bool)
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Function (($), (.), id)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Ix (inRange, range)
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

getDeviceRange :: IO (DeviceID, DeviceID)
getDeviceRange = (,) 0 . pred <$> countDevices

-- | Using DevideID that is out of range leads to Segfault
-- instead of normal excpetion...
isValidDeviceID :: DeviceID -> IO Bool
isValidDeviceID n = (`inRange` n) <$> getDeviceRange

getNumberedDeviceInfo :: DeviceID -> IO NumberedDeviceInfo
getNumberedDeviceInfo i = NumberedDeviceInfo i <$> getDeviceInfo i

listDevices :: IO [NumberedDeviceInfo]
listDevices = getDeviceRange >>= mapM getNumberedDeviceInfo . range

printDevices :: IO ()
printDevices = listDevices >>= mapM_ print

openInput' :: DeviceID -> IO PMStream
openInput' n = do
    b <- isValidDeviceID n
    unless b $ error "Device out of range"
    openInput n >>= \case
        Left s -> pure s
        Right e -> error (show e)

withInputStream :: DeviceID -> (PMStream -> IO c) -> IO c
withInputStream n = bracket (openInput' n) close

withEvents :: MonadIO m => Int -> (PMEvent -> m ()) -> PMStream -> m ()
withEvents n f s = forever $ do
    liftIO (readEvents s) >>= \case
        Left evs -> mapM_ f evs
        Right NoError -> pure ()
        Right e -> liftIO $ print e
    -- it is OK to check only so often, as eye won't process
    -- in at higher frequency than 100fps anyway...
    liftIO $ threadDelay n

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

-- Example
type ExampleState = StateT (Map.Map Int Int) IO

runExample :: ExampleState a -> IO a
runExample a = evalStateT a Map.empty

exampleProcEvent :: PMEvent -> ExampleState ()
exampleProcEvent PMEvent{..} =
    when (status == midiDown) $ do
        lift $ print (timestamp, dec)
        modify $ Map.insert k v
  where
    dec@PMMsg{..} = decodeMsg message

    k = fromIntegral data1

    v = fromIntegral data2
