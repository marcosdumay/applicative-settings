{-# LANGUAGE ExistentialQuantification, RankNTypes, GADTs #-}

{- |

What is expected here is:

1 - Construct a SettingsModel applicative using high level functions

2 - MyData <$> onKey "Field1" readField1 `description` "description 1" <*> onKey "Field2" readField2 :: SettingModel MyData

3 - moncat <$> mapM parseFile settingsModel [(reader, fileName)] :: IO (Setting MyData)

4 - retrieve setting :: Either [ReadStatus] MyData

-}
module ApSettings.Setting where

import Data.Text (Text)
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Monoid
import ApSettings.Values
import Data.Default.Class
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M


-- | Only the status information (error data or success) from reading the setting
type ReadStatus = Value ()

-- | A function that turns setting data into a useable Haskell value
type StructReader a = Key -> BareData -> Value a
mapStruct :: (a -> b) -> StructReader a -> StructReader b
mapStruct f ra = \k m -> let va = ra k m in f <$> va
type ScalarReader a = BareData -> Value a
mapScalar :: (a -> b) -> ScalarReader a -> ScalarReader b
mapScalar f ra = \bare -> let va = ra bare in f <$> va

{- |
A description of your settings

This is the setting you will define in your code. Settings are
Applicative, Functor and Alternative instances, so you can define
them applied over data constructors and obtain a well formed
settings data at the end.
-}
data Setting a =
  ConstSett (Maybe a) -- ^ Constant setting value, successful if Just, failed of Nothing
  | ScalarSett (ScalarReader a) -- ^ Reads a value directly from the base data
  | StructureSett (Setting a)
    {- ^
Declares a structure of settings, identified by keys.

It is not necessary to wrap structures in a StructureSett. Any instance of
KeySett already implies a structure. Instead, this
is used to group the structure into a single setting that can recieve a description
or a default value, marking it optional or other kinds of global effects.
-}
--  | ListSett (Setting a) -- ^ Unwraps a Multiple value when reading
  | KeySett Key (Setting a) -- ^ Does key lookup on structures
  | forall b. MultiSett (Setting (b -> a)) (Setting b) -- ^ Setting application
  | AltSett (Setting a) (Setting a) -- ^ Setting alternatives
  | DefSett a (Setting a) -- ^ Holds the default value of a setting
  | DocSett Text (Setting a) -- ^ Holds the description of a setting
  | LitSett Text (Setting a) -- ^ Hols the literary default value of a setting
instance Functor Setting where
  fmap f (ConstSett a) = ConstSett $ f <$> a
  fmap f (ScalarSett a) = ScalarSett $ mapScalar f a
  fmap f (StructureSett a) = StructureSett $ f <$> a
--  fmap f (ListSett a) = ListSett $ fmap f a
  fmap f (KeySett k r) = KeySett k $ f <$> r
  fmap f (MultiSett g a) = MultiSett ((f.) <$> g) a
  fmap f (AltSett a b) = AltSett (f <$> a) $ f <$> b
  fmap f (DefSett d m) = DefSett (f d) $ f <$> m
  fmap f (DocSett d m) = DocSett d $ f <$> m
  fmap f (LitSett d m) = LitSett d $ f <$> m
instance Applicative Setting where
  pure = ConstSett . Just
  (<*>) = MultiSett
instance Alternative Setting where
  empty = ConstSett Nothing
  (<|>) = AltSett

{- |
Declares a structure of settings identified by keys.

It is not necessary to declare structures. The mere use
of `onKey` already instructs the reader to fetch them.
Instead, structures declarations create an overall setting
that can receive descriptions, or default values, marked
as optional, set with options and other operations.
-}
structure :: Setting a -> Setting a
structure = StructureSett

onKey :: Key -> Setting a -> Setting a
onKey k r = KeySett k r

defVal :: Setting a -> a -> Setting a
defVal m v = DefSett v m

description :: Setting a -> Text -> Setting a
description m d = DocSett d m

optional :: Setting a -> Setting (Maybe a)
optional s = (Just <$> s) `defVal` Nothing

-- | After reading, those errors may appear
data SettingError =
  SettingNotFound
  | ParserError
  | ErrorInKey Key SettingError
  | ErrorInValue ValueError
  | MultipleErrors [SettingError]

catErrors :: SettingError -> SettingError -> SettingError
catErrors (MultipleErrors ee) e = MultipleErrors $ ee <> [e]
catErrors e (MultipleErrors ee) = MultipleErrors $ e : ee
catErrors a b = MultipleErrors [a, b]

readData :: Setting a -> BareData -> Either SettingError a
readData (ConstSett Nothing) _ = Left SettingNotFound
readData (ConstSett (Just a)) _ = Right a
readData (ScalarSett reader) d = unValue $ reader d
readData (StructureSett m) d = readData m d
readData (KeySett key reader) d = mapLeft (ErrorInKey key) $
  do
    v <- unValue $ unStructure d
    d' <- unValue . fromMaybe (Left ValueNotFound) $ Right <$> M.lookup key v
    readData reader d'
readData (MultiSett f x) d = let
  vf = readData f d
  vx = readData x d
  in apSett vf vx
  where
    apSett (Left a) (Left b) = Left $ catErrors a b
    apSett (Left a) (Right _) = Left a
    apSett (Right _) (Left b) = Left b
    apSett (Right a) (Right b) = Right $ a b
readData (AltSett a b) d = let
  sa = readData a d
  sb = readData b d
  in case sa of
    Left e -> if isEmptyError e then sb else sa
    _ -> sa
readData (DefSett a s) d = let
  vs = readData s d
  in case vs of
       Left e -> if isEmptyError e then pure a else vs
       _ -> vs
readData (DocSett _ s) d = readData s d
readData (LitSett _ s) d = readData s d

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left e) = Left $ f e
mapLeft _ (Right a) = Right a

unValue :: Value a -> Either SettingError a
unValue = mapLeft ErrorInValue

isEmptyError SettingNotFound = True
isEmptyError (ErrorInKey _ e) = isEmptyError e
isEmptyError (ErrorInValue ValueNotFound) = True
isEmptyError (MultipleErrors ee) = all isEmptyError ee