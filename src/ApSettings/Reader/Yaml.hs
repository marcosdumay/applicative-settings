module ApSettings.Reader.Yaml (
  parse
  ) where

import ApSettings.Values
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.HashMap.Lazy as M
import Data.Foldable (toList)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')

parse :: Text -> Maybe BareData
parse t = do
  ym <- Y.decode (encodeUtf8 t)
  obj <- case ym of
           (Y.Object o) -> Just o
           _ -> Nothing
  return $ subParse obj
  where
    subParse :: Y.Object -> BareData
    subParse = Structure . fmap parseElement
    parseElement :: Y.Value -> BareData
    parseElement (Y.Object o) = subParse o
    parseElement (Y.Array a) = Multiple . map parseElement . toList $ a
    parseElement (Y.String t) = Scalar $ TextualValue t
    parseElement (Y.Number n) = Scalar $ NumericValue n
    parseElement (Y.Bool b) = Scalar $ BooleanValue b
    parseElement Y.Null = Scalar EmptyValue

