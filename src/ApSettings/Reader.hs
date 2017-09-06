{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module ApSettings.Reader where

import Data.Monoid
import Data.Text (Text)
import Data.String (IsString, fromString)
import qualified Data.Text as T
import ApSettings.Values
import qualified ApSettings.Reader.Yaml as Y
import qualified Data.Scientific as Sci
import Data.Maybe (isJust)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Text.Read (readMaybe)
import qualified Data.Char as C

type SettingsReader = T.Text -> Maybe BareData

readYaml :: SettingsReader
readYaml = Y.parse

readable :: Read a => BareValue -> Value a
readable = r
  where
    r :: Read a => BareValue -> Value a
    r (UntypedText t) = tryread t
    r (TextualValue t) = tryread t
    r (NumericValue n) = readshow n
    r (BooleanValue b) = readshow b
    r EmptyValue = emptymsg

withText :: IsString t => (t -> Either Text a) -> BareValue -> Value a
withText f (UntypedText t) = case f . fromString . T.unpack $ t of
  Left e -> Left . ErrorMessage $ e
  Right v -> pure v
withText f (TextualValue t) = withText f $ UntypedText t
withText f (NumericValue n) = withText f . UntypedText . showText $ n
withText f (BooleanValue b) = withText f . UntypedText . showText $ b
withText f EmptyValue = emptymsg

integral :: (Read a, Integral a) => BareValue -> Value a
integral = i
  where
    convInt :: (Read a, Integral a) => Sci.Scientific -> Value a
    convInt n = case Sci.floatingOrInteger n of
      Right i -> Right i
      Left _ -> Left . ErrorMessage $ "expected integral number, found " <> showText n
    i (UntypedText t) = tryread t
    i (TextualValue t) = tryread t
    i (NumericValue n) = convInt n
    i (BooleanValue b) = Left . ErrorMessage $ "expected number, found boolean " <> showText b
    i EmptyValue = emptymsg

bounded :: (Show a, Ord a) => a -> a -> (BareValue -> Value a) -> BareValue -> Value a
bounded min max f v = b
  where
    -- b :: (Show a, Ord a) => Value a
    b = case f v of
             e@(Left _) -> e
             Right v' -> if v' < min
               then Left tooLarge
               else (if v' > max
                     then Left tooSmall
                     else Right v'
                    )
    tooLarge = ErrorMessage $ "value out of bounds, maximum was " <> showText max <> ", found " <> display v
    tooSmall = ErrorMessage $ "value out of bounds, minium was " <> showText min <> ", found " <> display v

realfrac :: RealFloat a => BareValue -> Value a
realfrac = rf
  where
    readsci :: Text -> Value Sci.Scientific
    readsci v = tryread v
    rf (UntypedText t) = Sci.toRealFloat <$> readsci t
    rf (TextualValue t) = Sci.toRealFloat <$> readsci t
    rf (NumericValue n) = pure $ Sci.toRealFloat n
    rf (BooleanValue b) = Left . ErrorMessage $ "expected number, found boolean " <> showText b
    rf EmptyValue = emptymsg
                                           

text :: IsString a => BareValue -> Value a
text = tx
  where
    conv = pure . fromString . T.unpack
    tx (UntypedText t) = conv t
    tx (TextualValue t) = conv t
    tx (NumericValue n) = Left . ErrorMessage $ "expected text, found number " <> showText n
    tx (BooleanValue b) = Left . ErrorMessage $ "expected text, found boolean " <> showText b
    tx EmptyValue = emptymsg

bool :: BareValue -> Value Bool
bool (UntypedText t) = let
  tlow = map C.toLower . T.unpack $ t
  isT = elem t ["1", "t", "true", "y", "yes"]
  isF = elem t ["0", "f", "false", "n", "no"]
  in if isT
     then pure True
     else if isF
          then pure False
          else Left . ErrorMessage $ "expected boolean, found " <> t
bool (TextualValue t) = bool (UntypedText t)
bool v@NumericValue{} = Left . ErrorMessage $ "expected boolean, found " <> display v
bool (BooleanValue b) = pure b
bool EmptyValue = emptymsg

optional :: (BareValue -> Value a) -> BareValue -> Value (Maybe a)
optional _ EmptyValue = pure Nothing
optional f v = Just <$> f v

tryread :: Read a => Text -> Value a
tryread t = case readMaybe . T.unpack $ t of
  Nothing -> Left . ErrorMessage $ "could not parse value " <> t
  Just v -> Right v

readshow :: (Show a, Read b) => a -> Value b
readshow a = case readMaybe . show $ a of
  Nothing -> Left . ErrorMessage $ "could not parse value " <> showText a
  Just v -> Right v

scalar :: (BareValue -> Value a) -> BareData -> Value a
scalar f (Scalar a) = f a
scalar _ Multiple{} = Left $ InvalidFormat "scalar" "list"
scalar _ Structure{} = Left $ InvalidFormat "scalar" "structure"

multiple :: (BareData -> Value a) -> BareData -> Value [a]
multiple f v@Scalar{} = (:[]) <$> f v
multiple f (Multiple aa) = mapM f aa
multiple f Structure{} = Left $ InvalidFormat "list" "structure"

onlyMultiple :: (BareData -> Value a) -> BareData -> Value [a]
onlyMultiple _ Scalar{} = Left $ InvalidFormat "list" "scalar"
onlyMultiple f a = multiple f a

showText :: Show a => a -> Text
showText = T.pack . show

emptymsg :: Value a
emptymsg = Left . ErrorMessage $ "null value"
