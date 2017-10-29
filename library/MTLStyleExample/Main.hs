module MTLStyleExample.Main
  ( main
  , mainIO
  ) where

import Prelude hiding (readFile, log)

import Control.Monad.Logger (logInfoN, runStderrLoggingT)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Environment as IO

import Data.Time.Clock (getCurrentTime, diffUTCTime)

import MTLStyleExample.Interfaces

--------------------------------------------------------------------------------
-- IO wiring

getArgsIO :: GetArgs IO
getArgsIO = map T.pack <$> IO.getArgs

readFileIO :: ReadFile IO
readFileIO = T.readFile . T.unpack

currentTimeIO :: CurrentTime IO
currentTimeIO = getCurrentTime

logIO :: Log IO
logIO = runStderrLoggingT . logInfoN

mainIO :: IO ()
mainIO = main (getArgsIO, readFileIO, logIO, currentTimeIO)

--------------------------------------------------------------------------------
-- Logic

main :: Monad m => (GetArgs m, ReadFile m, Log m, CurrentTime m) -> m ()
main (getArgs, readFile, log, currentTime) = do
  startTime <- currentTime
  [fileName] <- getArgs
  target <- readFile fileName
  log $ "Hello, " <> target <> "!"
  endTime <- currentTime
  let duration = endTime `diffUTCTime` startTime
  log $ T.pack (show (round (duration * 1000) :: Integer)) <> " milliseconds"


