{-# LANGUAGE FlexibleInstances #-}

module ApSettings.Reader where

import qualified Data.Text as T
import ApSettings.Values
import qualified ApSettings.Reader.Yaml as Y
import Data.Maybe (isJust)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')

type SettingsReader = T.Text -> Maybe BareData

readYaml :: SettingsReader
readYaml = Y.parse

