{-# LANGUAGE OverloadedStrings #-}

module ReadInputs where

import Data.Text (Text)
--import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import ApSettings

import Distribution.TestSuite
import Base

tests :: IO [Test]
tests = return [
  simpleTest "Reading 0" $ fromFile "test/inputs/input000.yaml" sett0 expected0,
  simpleTest "Reading 000" $ fromFile "test/inputs/input000.yaml" sett000 expected000
  ]



data Sett0 = Sett0 {s0k0 :: Text, s0k1:: Text, s0k2 :: [Text]} deriving (Show, Eq)
data Sett0003 = Sett0003 {key30 :: Text, key31 :: [Text]} deriving (Show, Eq)
data Sett000 = Sett000 {key0 :: Text, key1:: Text, key2 :: [Text],
                        key3 :: Sett0003, key4 :: Bool, key5 :: Int,
                        key6 :: String, key7 :: Double} deriving (Show, Eq)

sett0 :: Setting Sett0
sett0 = Sett0 <$>
  onKey "key0" `scalar` text <*>
  onKey "key1" `scalar` text <*>
  onKey "key2" `multiple` value text
expected0 :: Sett0
expected0 = Sett0 "value0" "value1" ["value20", "value21"]

sett000 :: Setting Sett000
sett000 = Sett000 <$>
  onKey "key0" `scalar` text <*>
  onKey "key1" `scalar` text <*>
  onKey "key2" `multiple` value text <*>
  onKey "key3" (Sett0003 <$>
                   onKey "key30" `scalar` text <*>
                   onKey "key31" `multiple` value text
               ) <*>
  onKey "key4" `scalar` bool <*>
  onKey "key5" `scalar` integral <*>
  onKey "key6" `scalar` text <*>
  onKey "key7" `scalar` real
expected000 :: Sett000
expected000 = Sett000 "value0" "value1" ["value20", "value21"] (
  Sett0003 "value30" ["value310", "value311"]
  ) True 100000000 "value 6 has spaces" 7.5

fromFile :: (Show a, Eq a) => String -> Setting a -> a -> IO Progress
fromFile file sett expected = do
    ym <- TIO.readFile file
    case readYaml ym of
      Nothing -> return . Finished . Fail $ "Could not parse settings file"
      Just bare -> case evaluateSetting sett [bare] of
        Left e -> return . Finished . Fail . show $ e
        Right sett' -> if sett' == expected then return . Finished $ Pass else return . Finished . Fail $ "Unexpected result: " ++ show sett'

