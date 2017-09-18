module ApSettings.Reader.Yaml (
  parse
  ) where

import ApSettings.Values
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Data.Foldable (toList)
import Data.Text.Encoding (encodeUtf8)

parse :: Text -> Either [Text] BareData
parse t = mapLeft (\x -> [T.pack x]) $ do
  ym <- Y.decodeEither (encodeUtf8 t)
  pure $ parseElement ym
  where
    subParse :: Y.Object -> BareData
    subParse = Structure . fmap parseElement
    parseElement :: Y.Value -> BareData
    parseElement (Y.Object o) = subParse o
    parseElement (Y.Array a) = Multiple . map parseElement . toList $ a
    parseElement (Y.String t') = Scalar $ TextualValue t'
    parseElement (Y.Number n) = Scalar $ NumericValue n
    parseElement (Y.Bool b) = Scalar $ BooleanValue b
    parseElement Y.Null = Scalar EmptyValue

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft _ (Right v) = Right v
mapLeft f (Left e) = Left . f $ e
