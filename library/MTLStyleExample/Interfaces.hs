module MTLStyleExample.Interfaces
  ( GetArgs
  , ReadFile
  , CurrentTime
  , Log
  ) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | Returns the command-line arguments provided to the program.
type GetArgs m = m [Text]

-- | Reads a file at the given path and returns its contents. If the file does
-- not exist, is not accessible, or is improperly encoded, this method throws
-- an exception.
type ReadFile m = Text -> m Text

-- | Get current time
type CurrentTime m = m UTCTime

-- | Logging
type Log m = Text -> m ()
