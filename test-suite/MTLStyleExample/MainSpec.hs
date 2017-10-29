module MTLStyleExample.MainSpec where

import Prelude hiding (log, readFile)

import Data.Functor.Identity (runIdentity)
import Control.Monad.Writer (runWriterT)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Test.Hspec

import MTLStyleExample.Main
import qualified MTLStyleExample.Test.Stubs as Stub

spec :: Spec
spec = describe "main" $ do
  let ((), logMessages) = runner $ main (getArgs, readFile, log, currentTime)
      getArgs = Stub.getArgs ["sample.txt"]
      readFile = Stub.readFile [("sample.txt", "Alyssa")]
      log = Stub.log
      currentTime = Stub.currentTime
      runner = runIdentity . runWriterT . Stub.runTickingClockT epoch
      epoch = posixSecondsToUTCTime 0

  it "prints two log messages" $
    length logMessages `shouldBe` 2

  it "prints a greeting as the first message" $
    (logMessages !! 0) `shouldBe` "Hello, Alyssa!"

  it "prints the elapsed time in milliseconds as the second message" $
    (logMessages !! 1) `shouldBe` "1000 milliseconds"
