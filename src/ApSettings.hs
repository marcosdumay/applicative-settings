{- |
#How to use

Define your settings structure and constructor, and create a parser in
applicative style. Read your settings files and other sources, and
evaluate the settings into your structure:

    import ApSettings

    data MySettings = MySettings {aSetting :: String, anotherSetting :: [Double], yetAnother :: Int}

    readSettings = do
        let settingsParser = MySettings <$>
            onKey "aSetting" `scalar` text <*>
            onKey "anotherSetting" `multiple` value real <*>
            onKey "yetAnother" `scalar` integral
        setts <- mapM readYaml ["file1", "file2", "file3"]
        pure $ evaluateSettings settingsParser setts

    main = do
        s' <- readSettings
        case s' of
	    Left e -> hPutStr stderr e
	    Right s -> do some stuff with your (s :: MySettings)
-}

module ApSettings (
  Key,
  ValueError(..),
  SettingsReader,
  withText,
  integral,
  bounded,
  real,
  text,
  bool,
  readable,
  ScalarReader,
  Setting(..),
  onKey,
  defaultTo,
  value,
  scalar,
  multiple,
  evalSett,
  readYaml,
  fromFile
  ) where

import ApSettings.Values
import ApSettings.Reader
import ApSettings.Setting
import qualified ApSettings.Reader.Yaml as Y
import Data.Text (Text)
import qualified Data.Text.IO as TIO

-- | Reads an YAML file into unparsed data
readYaml :: SettingsReader
readYaml = Y.parse

fromFile :: String -> IO Text
fromFile = TIO.readFile
