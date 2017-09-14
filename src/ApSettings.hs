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
  optional,
  readable,
  ScalarReader,
  Setting(..),
  onKey,
  defaultTo,
  value,
  scalar,
  multiple,
  evaluateSetting,
  readYaml
  ) where

import ApSettings.Values
import ApSettings.Reader
import ApSettings.Setting
import qualified ApSettings.Reader.Yaml as Y

-- | Reads an YAML file into unparsed data
readYaml :: SettingsReader
readYaml = Y.parse

