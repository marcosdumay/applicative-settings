{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module ApSettings.Values where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific (Scientific)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M

-- | Setting keys
type Key = Text

-- | Setting descriptions
type Description = Text

-- | An unparsed scalar value
data BareValue =
  UntypedText Text |
  TextualValue Text |
  NumericValue Scientific |
  BooleanValue Bool |
  EmptyValue deriving (Read, Show, Eq, Ord)

-- | Unparsed data
data BareData =
  Scalar BareValue |
  Multiple [BareData] |
  Structure (M.HashMap Key BareData) deriving (Read, Show, Eq)

-- | Errors that may be found when parsing values
data ValueError =
  ValueNotFound -- ^ There is no value
  | InvalidFormat Text Text -- ^ The data format is different from the expected: `InvalidFormat expected found`
  | ErrorMessage Text -- ^ An error message on reading
  deriving (Read, Show, Eq, Ord)

-- | Error display of unparsed scalars
display :: BareValue -> Text
display (UntypedText t) = t
display (TextualValue t) = t
display (NumericValue n) = T.pack . show $ n
display (BooleanValue b) = if b then "true" else "false"
display EmptyValue = "null"

-- | Parsed values
type Value a = Either ValueError a

instance Monoid (Value a) where
  mempty = Left ValueNotFound
  mappend (Left ValueNotFound) b = b
  mappend a _ = a

-- | Error display of scalar parsing results
statusMessage :: ValueError -> Text
statusMessage ValueNotFound = "not found"
statusMessage (InvalidFormat a b) = "invalid format, expected " <> a <> ", found " <> b
statusMessage (ErrorMessage m) = m

-- | Fetches a map of key -> unparsed value from unparsed data
unStructure :: BareData -> Either ValueError (HashMap Key BareData)
unStructure (Structure s) = Right s
unStructure (Multiple{}) = Left (InvalidFormat "structure" "list")
unStructure (Scalar{}) = Left (InvalidFormat "structure" "scalar")

-- | Fetches an unparsed scalar from unparsed data
unScalar :: BareData -> Either ValueError BareValue
unScalar (Scalar s) = Right s
unScalar (Multiple{}) = Left $ InvalidFormat "scalar" "list"
unScalar (Structure{}) = Left $ InvalidFormat "scalar" "structure"

-- | Fetches a list of data from unparsed data
unMultiple :: BareData -> Either ValueError [BareData]
unMultiple (Multiple m) = Right m
unMultiple (Scalar{}) = Left $ InvalidFormat "list" "scalar"
unMultiple (Structure{}) = Left $ InvalidFormat "list" "structire"
