{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module ApSettings.Values where

import Data.Monoid ((<>))
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific (Scientific)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M

type Key = Text
type Description = Text

data BareValue =
  UntypedText Text |
  TextualValue Text |
  NumericValue Scientific |
  BooleanValue Bool |
  EmptyValue

data BareData =
  Scalar BareValue |
  Multiple [BareData] |
  Structure (M.HashMap Key BareData)


data ValueError =
  ValueNotFound -- ^ There is no value
  | InvalidFormat Text Text -- ^ The data format is different from the expected: `InvalidFormat expected found`
  | ErrorMessage Text -- ^ An error message on reading

display :: BareValue -> Text
display (TextualValue t) = T.pack . show $ t
display (NumericValue n) = T.pack . show $ n
display (BooleanValue b) = if b then "true" else "false"
display EmptyValue = "null"

type Value a = Either ValueError a

instance Monoid (Value a) where
  mempty = Left ValueNotFound
  mappend (Left ValueNotFound) b = b
  mappend a _ = a

statusMessage :: ValueError -> Text
statusMessage ValueNotFound = "not found"
statusMessage (InvalidFormat a b) = "invalid format, expected " <> a <> ", found " <> b

unStructure :: BareData -> Either ValueError (HashMap Key BareData)
unStructure (Structure s) = Right s
unStructure (Multiple{}) = Left (InvalidFormat "structure" "list")
unStructure (Scalar{}) = Left (InvalidFormat "structure" "scalar")

unScalar :: BareData -> Either ValueError BareValue
unScalar (Scalar s) = Right s
unScalar (Multiple{}) = Left $ InvalidFormat "scalar" "list"
unScalar (Structure{}) = Left $ InvalidFormat "scalar" "structure"

unMultiple :: BareData -> Either ValueError [BareData]
unMultiple (Multiple m) = Right m
unMultiple (Scalar{}) = Left $ InvalidFormat "list" "scalar"
unMultiple (Structure{}) = Left $ InvalidFormat "list" "structire"
