{-# LANGUAGE ExistentialQuantification, RankNTypes, FlexibleInstances #-}

module ApSettings.Setting where

import Data.Text (Text)
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Monoid
import ApSettings.Values
--import Data.Default.Class
--import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M

import Debug.Trace


type ScalarReader a = BareValue -> Value a
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
  | forall b. ListSett ([b] -> a) (Setting b) -- ^ Unwraps a Multiple value when reading
  | KeySett Key (Setting a) -- ^ Does key lookup on structures
  | forall b. MultiSett (Setting (b -> a)) (Setting b) -- ^ Setting application
  | AltSett (Setting a) (Setting a) -- ^ Setting alternatives
  | DefSett a (Setting a) -- ^ Holds the default value of a setting
  | DocSett Text (Setting a) -- ^ Holds the description of a setting
  | LitSett Text (Setting a) -- ^ Holds the literal default value of a setting
instance Functor Setting where
  fmap f (ConstSett a) = ConstSett $ f <$> a
  fmap f (ScalarSett a) = ScalarSett $ mapScalar f a
  fmap f (StructureSett a) = StructureSett $ f <$> a
  -- <$> :: (a -> b) -> Sett a -> Sett b
  -- (?) :: ([a] -> [b]) -> (a -> b)
  fmap f (ListSett g a) = ListSett (f.g) a
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

{- |
Deferences keys on a structure, reading their value.
-}
onKey :: Key -> Setting a -> Setting a
onKey k r = KeySett k r

{- |
Sets the default value of a setting.

The default value is used only when a setting is
not found, not on other errors. Also, Default
values are applied before alternatives are decided.
-}
defaultTo :: Setting a -> a -> Setting a
defaultTo m v = DefSett v m

{- |
Adds a desctiption into the setting.

This has no effect on reading.
-}
description :: Setting a -> Text -> Setting a
description m d = DocSett d m

{- |
Creates a setting out of a scalar value reader.
-}
value :: ScalarReader a -> Setting a
value = ScalarSett

{- |
Instructs a partial setting to read a scalar value.
-}
scalar :: (Setting a -> Setting a) -> ScalarReader a -> Setting a
scalar s r = s $ ScalarSett r

{- |
Instructs a partial setting to read a list of values.
-}
multiple :: (Setting [a] -> Setting [a]) -> Setting a -> Setting [a]
multiple s r = s $ ListSett id r

-- | After reading, those errors may appear
data SettingError =
  SettingNotFound
  | ParserError
  | ErrorInKey Key SettingError
  | ErrorInValue ValueError
  | MultipleErrors [SettingError] deriving (Read, Show, Eq, Ord)
instance Monoid (Either SettingError a) where
  mempty = Left SettingNotFound
  mappend a@(Left e) b = if isEmptyError e then b else a
  mappend a _ = a


catErrors :: SettingError -> SettingError -> SettingError
catErrors (MultipleErrors ee) e = MultipleErrors $ ee <> [e]
catErrors e (MultipleErrors ee) = MultipleErrors $ e : ee
catErrors a b = MultipleErrors [a, b]

readData :: Setting a -> [BareData] -> Either SettingError a
readData (ConstSett Nothing) = const $ Left SettingNotFound
readData (ConstSett (Just a)) = const $ Right a
readData (ScalarSett reader) = foldMap $ \d ->
  do
    r <- unValue $ unScalar d
    unValue $ reader r
readData (ListSett f s) = \d -> do
    dd <- mapM (unValue . unMultiple) d
    -- dd :: [[BareValue]]
    -- Now, what does that mean?
    -- I must take the first list available in the settings,
    -- and drop anything else. Unless that is empty too,
    -- in case I take the second element, and so on.
    let unwrap ll = case ll of
          [] -> Left . ErrorInValue $ ValueNotFound
          (d':dd') -> case mapM (readData s) $ map (\x -> [x]) d' of
            Left e -> if isEmptyError e then unwrap dd' else Left e
            Right vv -> traceShow (length vv) $ pure $ f vv
    traceShow dd $ unwrap dd
readData (StructureSett m) = readData m
readData (KeySett key reader) = \d -> mapLeft (ErrorInKey key) $
  do
    v <- mapM (unValue . unStructure) d
    let
      lkup :: M.HashMap Key BareData -> Either SettingError BareData
      lkup val = let
        v' = M.lookup key val :: Maybe BareData
        ve = fromMaybe (Left ValueNotFound) $ Right <$> v' :: Either ValueError BareData
        in unValue ve
    d' <- mapM lkup v
    readData reader d'
readData (MultiSett f x) = \d -> let
  vf = readData f d
  vx = readData x d
  in apSett vf vx
  where
    apSett (Left a) (Left b) = Left $ catErrors a b
    apSett (Left a) (Right _) = Left a
    apSett (Right _) (Left b) = Left b
    apSett (Right a) (Right b) = Right $ a b
readData (AltSett a b) = \d -> let
  sa = readData a d
  sb = readData b d
  in case sa of
    Left e -> if isEmptyError e then sb else sa
    _ -> sa
readData (DefSett a s) = \d -> let
  vs = readData s d
  in case vs of
       Left e -> if isEmptyError e then pure a else vs
       _ -> vs
readData (DocSett _ s) = readData s
readData (LitSett _ s) = readData s

-- | Evaluates a list of settings 
evaluateSetting :: Setting a -> [BareData] -> Either SettingError a
evaluateSetting s dt = evaluate' dt []
  where
    -- Accumulates read data at the seccond parameter until the result
    -- is not empty anymore.
    --evaluate' :: [[BareData]] -> [[BareData]] -> Either SettingError a
    evaluate' [] [] = Left SettingNotFound
    evaluate' [] o = readData s o
    evaluate' (n:nn) o = case readData s o of
           m@(Left e) -> if isEmptyError e
             then let
             no = o ++ [n]
             in evaluate' nn no
             else m
           v@(Right _) -> v

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left e) = Left $ f e
mapLeft _ (Right a) = Right a

unValue :: Value a -> Either SettingError a
unValue = mapLeft ErrorInValue

isEmptyError :: SettingError -> Bool
isEmptyError SettingNotFound = True
isEmptyError (ErrorInKey _ e) = isEmptyError e
isEmptyError (ErrorInValue ValueNotFound) = True
isEmptyError (MultipleErrors ee) = all isEmptyError ee
isEmptyError _ = False

foldMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
foldMapM _ [] = pure mempty
foldMapM f (a : aa) = do
  a' <- f a
  aa' <- foldMapM f aa
  pure $ a' <> aa'
