{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module ApSettings.Reader where

import Data.Monoid
import Data.Text (Text)
import Data.String (IsString, fromString)
import qualified Data.Text as T
import ApSettings.Values
--import qualified ApSettings.Reader.Yaml as Y
import qualified Data.Scientific as Sci
--import Data.Maybe (isJust)
--import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Text.Read (readMaybe)
import qualified Data.Char as C

type SettingsReader = T.Text -> Maybe BareData

-- | Parses the textual representation of a scalar with a user-supplied function
withText :: IsString t => (t -> Either Text a) -> BareValue -> Value a
withText f (UntypedText t) = case f . fromString . T.unpack $ t of
  Left e -> valueError . ErrorMessage $ e
  Right v -> pure v
withText f (TextualValue t) = withText f $ UntypedText t
withText f (NumericValue n) = withText f . UntypedText . showText $ n
withText f (BooleanValue b) = withText f . UntypedText . showText $ b
withText _ EmptyValue = emptymsg

-- | Parses an integral from a scalar
integral :: Integral a => BareValue -> Value a
integral = i
  where
    convInt :: Integral a => Sci.Scientific -> Value a
    convInt n = case Sci.floatingOrInteger n of
      Right i' -> pure i'
      Left d -> let _ = d :: Double in valueError . ErrorMessage $ "expected integral number, found " <> showText n
    i (UntypedText t) = tryread t >>= convInt
    i (TextualValue t) = tryread t >>= convInt
    i (NumericValue n) = convInt n
    i (BooleanValue b) = valueError . ErrorMessage $ "expected number, found boolean " <> showText b
    i EmptyValue = emptymsg

-- | Bounds a parsed value within a maximum and a minimum.
bounded :: (Show a, Ord a) => a -> a -> (BareValue -> Value a) -> BareValue -> Value a
bounded min' max' f v = b
  where
    -- b :: (Show a, Ord a) => Value a
    b = case f v of
             e@(Value (Left _)) -> e
             Value (Right v') -> if v' < min'
               then tooLarge
               else (if v' > max'
                     then tooSmall
                     else pure v'
                    )
    tooLarge = valueError . ErrorMessage $ "value out of bounds, maximum was " <> showText max' <> ", found " <> display v
    tooSmall = valueError . ErrorMessage $ "value out of bounds, minium was " <> showText min' <> ", found " <> display v

-- | Parses a real number from a scalar
real :: RealFloat a => BareValue -> Value a
real = rf
  where
    readsci :: Text -> Value Sci.Scientific
    readsci v = tryread v
    rf (UntypedText t) = Sci.toRealFloat <$> readsci t
    rf (TextualValue t) = Sci.toRealFloat <$> readsci t
    rf (NumericValue n) = pure $ Sci.toRealFloat n
    rf (BooleanValue b) = valueError . ErrorMessage $ "expected number, found boolean " <> showText b
    rf EmptyValue = emptymsg

-- | Parses text from a scalar
text :: IsString a => BareValue -> Value a
text = tx
  where
    conv = pure . fromString . T.unpack
    tx (UntypedText t) = conv t
    tx (TextualValue t) = conv t
    tx (NumericValue n) = valueError . ErrorMessage $ "expected text, found number " <> showText n
    tx (BooleanValue b) = valueError . ErrorMessage $ "expected text, found boolean " <> showText b
    tx EmptyValue = emptymsg

-- | Parses a boolean from a scalar
bool :: BareValue -> Value Bool
bool (UntypedText t) = let
  tlow = map C.toLower . T.unpack $ t
  isT = elem tlow ["1", "t", "true", "y", "yes"]
  isF = elem tlow ["0", "f", "false", "n", "no"]
  in if isT
     then pure True
     else if isF
          then pure False
          else valueError . ErrorMessage $ "expected boolean, found " <> t
bool (TextualValue t) = bool (UntypedText t)
bool v@NumericValue{} = valueError . ErrorMessage $ "expected boolean, found " <> display v
bool (BooleanValue b) = pure b
bool EmptyValue = emptymsg

-- | Parses a scalar with the `read` function.
readable :: Read a => BareValue -> Value a
readable (UntypedText t) = tryread t
readable (TextualValue t) = tryread t
readable (NumericValue n) = readshow n
readable (BooleanValue b) = readshow b
readable EmptyValue = emptymsg

tryread :: Read a => Text -> Value a
tryread t = case readMaybe . T.unpack $ t of
  Nothing -> valueError . ErrorMessage $ "could not parse value " <> t
  Just v -> pure v

readshow :: (Show a, Read b) => a -> Value b
readshow a = case readMaybe . show $ a of
  Nothing -> valueError . ErrorMessage $ "could not parse value " <> showText a
  Just v -> pure v

showText :: Show a => a -> Text
showText = T.pack . show

emptymsg :: Value a
emptymsg = valueError . ErrorMessage $ "null value"
