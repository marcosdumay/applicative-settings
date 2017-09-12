{-# LANGUAGE OverloadedStrings #-}

module Base (simpleTest, noTimeoutTest) where

import Distribution.TestSuite
import System.IO.Error
import Data.Maybe
import System.Timeout

mktest :: String -> IO Progress -> Test
mktest n t = let
  test = TestInstance {
    run = t,
    name = n,
    tags = [],
    options = [],
    setOption = \_ _ -> Right test
    }
  in Test test

simpleTest :: String -> IO Progress -> Test
simpleTest n t = mktest n . catchTest . timeoutTest $ t

noTimeoutTest :: String -> IO Progress -> Test
noTimeoutTest n t = mktest n . catchTest $ t

catchTest :: IO Progress -> IO Progress
catchTest t = catchIOError t (
  \e -> return . Finished . Fail $ "Raised exception: " ++ show e
  )

timeoutTest :: IO Progress -> IO Progress
timeoutTest t = fromMaybe (Finished . Fail $ "Timeout!") <$> (
  timeout 1000000 t
  )
