{-# LANGUAGE ExistentialQuantification, RankNTypes, FlexibleInstances, GeneralizedNewtypeDeriving, DeriveTraversable, OverloadedStrings #-}

module ApSettings.Setting where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Monoid
import ApSettings.Values
import qualified Data.HashMap.Lazy as M

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

newtype SettingResult a = SettingResult (Either SettingError a)
  deriving (
    Applicative,
    Functor,
    Monad,
    Show,
    Read,
    Foldable,
    Traversable,
    Eq,
    Ord)

settingError :: SettingError -> SettingResult a
settingError e = SettingResult . Left $ e

instance Monoid (SettingResult a) where
  mempty = settingError SettingNotFound
  mappend a@(SettingResult (Left e)) b = if isEmptyError e then b else a
  mappend a _ = a
instance Alternative SettingResult where
  empty = mempty
  (<|>) = mappend

catErrors :: SettingError -> SettingError -> SettingError
catErrors (MultipleErrors ee) e = MultipleErrors $ ee <> [e]
catErrors e (MultipleErrors ee) = MultipleErrors $ e : ee
catErrors a b = MultipleErrors [a, b]

readData :: Setting a -> [BareData] -> SettingResult a
readData (ConstSett Nothing) = const $ settingError SettingNotFound
readData (ConstSett (Just a)) = const $ pure a
readData (ScalarSett reader) = foldMap $ \d ->
  do
    r <- unEVal $ unScalar d
    unValue $ reader r
readData (ListSett f s) = \d -> do
    dd <- mapM (unEVal . unMultiple) d
    -- dd :: [[BareValue]]
    -- Now, what does that mean?
    -- I must take the first list available in the settings,
    -- and drop anything else. Unless that is empty too,
    -- in case I take the second element, and so on.
    let unwrap ll = case ll of
          [] -> settingError . ErrorInValue $ ValueNotFound
          (d':dd') -> case mapM (readData s) $ map (\x -> [x]) d' of
            SettingResult (Left e) -> if isEmptyError e then unwrap dd' else settingError e
            SettingResult (Right vv) -> pure $ f vv
    unwrap dd
readData (StructureSett m) = readData m
readData (KeySett key reader) = \d -> mapSErr (ErrorInKey key) $
  do
    v <- mapM (unEVal . unStructure) d
    let
      lkup :: M.HashMap Key BareData -> SettingResult BareData
      lkup val = let
        v' = M.lookup key val :: Maybe BareData
        ve = fromMaybe (Left ValueNotFound) $ Right <$> v' :: Either ValueError BareData
        in unEVal ve
    d' <- mapM lkup v
    readData reader d'
readData (MultiSett f x) = \d -> let
  vf = readData f d
  vx = readData x d
  in apSett vf vx
  where
    apSett (SettingResult (Left a)) (SettingResult (Left b)) = settingError $ catErrors a b
    apSett (SettingResult (Left a)) (SettingResult (Right _)) = settingError a
    apSett (SettingResult (Right _)) (SettingResult (Left b)) = settingError b
    apSett (SettingResult (Right a)) (SettingResult (Right b)) = pure $ a b
readData (AltSett a b) = \d -> let
  sa = readData a d
  sb = readData b d
  in case sa of
    SettingResult (Left e) -> if isEmptyError e then sb else sa
    _ -> sa
readData (DefSett a s) = \d -> let
  vs = readData s d
  in case vs of
       SettingResult (Left e) -> if isEmptyError e then pure a else vs
       _ -> vs
readData (DocSett _ s) = readData s
readData (LitSett _ s) = readData s

-- | Evaluates a list of settings 
evalSett :: Setting a -> [BareData] -> Either [Text] a
evalSett s dt = case r of
  SettingResult (Left e) -> Left $ errorMessage e
  SettingResult (Right v) -> Right v
  where
    r = evaluate' dt []
    -- Accumulates read data at the seccond parameter until the result
    -- is not empty anymore.
    --evaluate' :: [[BareData]] -> [[BareData]] -> Either SettingError a
    evaluate' [] [] = settingError SettingNotFound
    evaluate' [] o = readData s o
    evaluate' (n:nn) o = case readData s o of
           m@(SettingResult (Left e)) -> if isEmptyError e
             then let
             no = o ++ [n]
             in evaluate' nn no
             else m
           v@(SettingResult (Right _)) -> v

mapErr :: (a -> SettingError) -> Either a b -> SettingResult b
mapErr f (Left e) = settingError $ f e
mapErr _ (Right a) = pure a

mapSErr :: (SettingError -> SettingError) -> SettingResult a -> SettingResult a
mapSErr f (SettingResult (Left e)) = settingError $ f e
mapSErr _ v@(SettingResult (Right _)) = v

unEVal :: Either ValueError a -> SettingResult a
unEVal (Left e) = settingError . ErrorInValue $ e
unEVal (Right v) = pure v

unValue :: Value a -> SettingResult a
unValue (Value (Right v)) = pure v
unValue (Value (Left e)) = settingError . ErrorInValue $ e

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

errorMessage :: SettingError -> [Text]
errorMessage SettingNotFound = ["setting not found"]
errorMessage ParserError = ["parser error"]
errorMessage (ErrorInKey k e) = ["in key " <> k <> ": " <> emsg]
  where
    emsg = T.intercalate ", " $ errorMessage e 
errorMessage (ErrorInValue e) = [statusMessage e]
errorMessage (MultipleErrors ee) = concat $ fmap errorMessage ee
