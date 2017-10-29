module MTLStyleExample.Test.Stubs where

import qualified Data.Text as T

import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Control.Monad.Writer (tell, MonadWriter)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import System.Log.FastLogger (fromLogStr, toLogStr)

import MTLStyleExample.Interfaces

--------------------------------------------------------------------------------
-- Arguments

-- | Runs a computation with access to a set of command-line arguments.
getArgs :: Monad m => [Text] -> GetArgs m
getArgs = return

--------------------------------------------------------------------------------
-- File System

-- | Runs a computation that may interact with the file system, given a mapping
-- from file paths to file contents.
readFile :: Monad m => [(Text, Text)] -> ReadFile m
readFile files path = 
  maybe (fail $ "readFile: no such file ‘" ++ T.unpack path ++ "’")
        return (lookup path files)

--------------------------------------------------------------------------------
-- Logger

-- | Runs a computation that may emit log messages, returning the result of the
-- computation combined with the set of messages logged, in order.
log :: MonadWriter [ByteString] m => Log m
log str = tell [fromLogStr (toLogStr str)]

--------------------------------------------------------------------------------
-- Clock

data ClockState
  = ClockStopped !UTCTime
  | ClockTick !UTCTime ClockState
  | ClockEndOfTime
  deriving (Eq, Show)

type ClockT m = StateT ClockState m

currentTime :: MonadState ClockState m => CurrentTime m
currentTime = get >>= \case
  ClockStopped t -> return t
  ClockTick t s -> put s >> return t
  ClockEndOfTime -> fail "currentTime: end of time"

-- | Runs a computation with a constant time that never changes.
runStoppedClockT :: (Monad m) => UTCTime -> ClockT m a -> m a
runStoppedClockT t x = evalStateT x (ClockStopped t)

-- | Runs a computation with a clock that advances by 1 second every time the
-- time is read.
runTickingClockT :: Monad m => UTCTime -> ClockT m a -> m a
runTickingClockT = runTickingClockT' 1

-- | Runs a computation with a clock that advances by the given interval every
-- time the time is read.
runTickingClockT' :: Monad m => NominalDiffTime -> UTCTime -> ClockT m a -> m a
runTickingClockT' d t x = evalStateT x (ticks t)
  where ticks t' = ClockTick t' (ticks (addUTCTime d t'))

-- | Runs a computation with a clock that replays the provided list of times, in
-- order. If the time list of times is exhausted, 'currentTime' will throw an
-- exception the next time it is called.
runPresetClockT :: Monad m => [UTCTime] -> ClockT m a -> m a
runPresetClockT ts x = evalStateT x (foldr ClockTick ClockEndOfTime ts)
