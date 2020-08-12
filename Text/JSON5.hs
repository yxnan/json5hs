{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances #-}
-- | Serialising Haskell values to and from JSON5 values.
module Text.JSON5 (
    -- * JSON5 Types
    JSValue(..)

    -- * Serialization to and from JSValues
  , JSON5(..)

    -- * Encoding and Decoding
  , Result(..)
  , encode -- :: JSON5 a => a -> String
  , decode -- :: JSON5 a => String -> Either String a
  , encodeStrict -- :: JSON5 a => a -> String
  , decodeStrict -- :: JSON5 a => String -> Either String a

    -- * Wrapper Types
  , JSString
  , toJSString
  , fromJSString

  , JSObject
  , toJSObject
  , fromJSObject
  , resultToEither

    -- * Serialization to and from Strings.
    -- ** Reading JSON5
  , readJSNull, readJSBool, readJSString, readJSRational
  , readJSArray, readJSObject, readJSValue

    -- ** Writing JSON5
  , showJSNull, showJSBool, showJSArray
  , showJSRational, showJSInfNaN
  , showJSObject, showJSValue

    -- ** Instance helpers
  , makeObj, valFromObj
  , JSKey(..), encJSDict, decJSDict

  ) where

import Text.JSON5.Types
import Text.JSON5.String

import Data.Int
import Data.Word
import Control.Monad(liftM, ap, MonadPlus(..))
import Control.Applicative

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntSet as I
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.IntMap as IntMap

import qualified Data.Array as Array
import qualified Data.Text as T

------------------------------------------------------------------------

-- | Decode a String representing a JSON5 value
-- (either an object, array, bool, number, null)
--
-- This is a superset of JSON5, as types other than
-- Array and Object are allowed at the top level.
--
decode :: (JSON5 a) => String -> Result a
decode s = case runGetJSON readJSValue s of
             Right a  -> readJSON a
             Left err -> Error err

-- | Encode a Haskell value into a string, in JSON5 format.
--
-- This is a superset of JSON5, as types other than
-- Array and Object are allowed at the top level.
--
encode :: (JSON5 a) => a -> String
encode = (flip showJSValue [] . showJSON)

------------------------------------------------------------------------

-- | Decode a String representing a strict JSON value.
-- This follows the spec, and requires top level
-- JSON5 types to be an Array or Object.
decodeStrict :: (JSON5 a) => String -> Result a
decodeStrict s = case runGetJSON readJSTopType s of
     Right a  -> readJSON a
     Left err -> Error err

-- | Encode a value as a String in strict JSON format.
-- This follows the spec, and requires all values
-- at the top level to be wrapped in either an Array or Object.
-- JSON5 types to be an Array or Object.
encodeStrict :: (JSON5 a) => a -> String
encodeStrict = (flip showJSTopType [] . showJSON)

------------------------------------------------------------------------

-- | The class of types serialisable to and from JSON5
class JSON5 a where
  readJSON  :: JSValue -> Result a
  showJSON  :: a -> JSValue

  readJSONs :: JSValue -> Result [a]
  readJSONs (JSArray as) = mapM readJSON as
  readJSONs _            = mkError "Unable to read list"

  showJSONs :: [a] -> JSValue
  showJSONs = JSArray . map showJSON

-- | A type for parser results
data Result a = Ok a | Error String
  deriving (Eq,Show)

-- | Map Results to Eithers
resultToEither :: Result a -> Either String a
resultToEither (Ok a)    = Right a
resultToEither (Error s) = Left  s

instance Functor Result where
  fmap = liftM

instance Applicative Result where
  (<*>) = ap
  pure  = return

instance Alternative Result where
  Ok a    <|> _ = Ok a
  Error _ <|> b = b
  empty         = Error "empty"

instance MonadPlus Result where
  Ok a `mplus` _ = Ok a
  _ `mplus` x    = x
  mzero          = Error "Result: MonadPlus.empty"

instance Monad Result where
  return x      = Ok x
  Ok a >>= f    = f a
  Error x >>= _ = Error x

instance MonadFail Result where
  fail x        = Error x

mkError :: String -> Result a
mkError s = Error s

--------------------------------------------------------------------
--
-- | To ensure we generate valid JSON5, we map Haskell types to JSValue
-- internally, then pretty print that.
--
instance JSON5 JSValue where
    showJSON = id
    readJSON = return

second :: (a -> b) -> (x,a) -> (x,b)
second f (a,b) = (a, f b)

--------------------------------------------------------------------
-- Some simple JSON5 wrapper types, to avoid overlapping instances

-- instance JSON5 JSNumber where
--   readJSON (JSRational r) = return r
--   readJSON (JSInfNaN n)   = return n
--   readJSON _              = mkError "Unable to read JSNumber"
--   showJSON = JSNumber

instance JSON5 JSString where
  readJSON (JSString s) = return s
  readJSON _            = mkError "Unable to read JSString"
  showJSON = JSString

instance (JSON5 a) => JSON5 (JSObject a) where
  readJSON (JSObject o) =
      let f (x,y) = do y' <- readJSON y; return (x,y')
      in toJSObject `fmap` mapM f (fromJSObject o)
  readJSON _ = mkError "Unable to read JSObject"
  showJSON = JSObject . toJSObject . map (second showJSON) . fromJSObject


-- -----------------------------------------------------------------
-- Instances
--

instance JSON5 Bool where
  showJSON = JSBool
  readJSON (JSBool b) = return b
  readJSON _          = mkError "Unable to read Bool"

instance JSON5 Char where
  showJSON  = JSString . toJSString . (:[])
  showJSONs = JSString . toJSString

  readJSON (JSString s) = case fromJSString s of
                            [c] -> return c
                            _ -> mkError "Unable to read Char"
  readJSON _            = mkError "Unable to read Char"

  readJSONs (JSString s)  = return (fromJSString s)
  readJSONs (JSArray a)   = mapM readJSON a
  readJSONs _             = mkError "Unable to read String"

instance JSON5 Ordering where
  showJSON = encJSString show
  readJSON = decJSString "Ordering" readOrd
    where
     readOrd x =
       case x of
         "LT" -> return Prelude.LT
         "EQ" -> return Prelude.EQ
         "GT" -> return Prelude.GT
         _    -> mkError ("Unable to read Ordering")

-- -----------------------------------------------------------------
-- Integral types

instance JSON5 Integer where
  showJSON = fromJSRational . fromIntegral
  readJSON (JSNumber (JSRational i)) = return $ round i
  readJSON _ = mkError "Unable to read Integer"

-- constrained:
instance JSON5 Int where
  showJSON = fromJSRational . fromIntegral
  readJSON (JSNumber (JSRational i)) = return $ round i
  readJSON _ = mkError "Unable to read Int"

-- constrained:
instance JSON5 Word where
  showJSON = fromJSRational . toRational
  readJSON (JSNumber (JSRational i)) = return $ truncate i
  readJSON _ = mkError "Unable to read Word"

-- -----------------------------------------------------------------

instance JSON5 Word8 where
  showJSON = fromJSRational . fromIntegral
  readJSON (JSNumber (JSRational i)) = return $ truncate i
  readJSON _ = mkError "Unable to read Word8"

instance JSON5 Word16 where
  showJSON = fromJSRational . fromIntegral
  readJSON (JSNumber (JSRational i)) = return $ truncate i
  readJSON _ = mkError "Unable to read Word16"

instance JSON5 Word32 where
  showJSON = fromJSRational . fromIntegral
  readJSON (JSNumber (JSRational i)) = return $ truncate i
  readJSON _ = mkError "Unable to read Word32"

instance JSON5 Word64 where
  showJSON = fromJSRational . fromIntegral
  readJSON (JSNumber (JSRational i)) = return $ truncate i
  readJSON _ = mkError "Unable to read Word64"

instance JSON5 Int8 where
  showJSON = fromJSRational . fromIntegral
  readJSON (JSNumber (JSRational i)) = return $ truncate i
  readJSON _ = mkError "Unable to read Int8"

instance JSON5 Int16 where
  showJSON = fromJSRational . fromIntegral
  readJSON (JSNumber (JSRational i)) = return $ truncate i
  readJSON _ = mkError "Unable to read Int16"

instance JSON5 Int32 where
  showJSON = fromJSRational . fromIntegral
  readJSON (JSNumber (JSRational i)) = return $ truncate i
  readJSON _ = mkError "Unable to read Int32"

instance JSON5 Int64 where
  showJSON = fromJSRational . fromIntegral
  readJSON (JSNumber (JSRational i)) = return $ truncate i
  readJSON _ = mkError "Unable to read Int64"

-- -----------------------------------------------------------------

instance JSON5 Double where
  showJSON = fromJSRational . toRational
  readJSON (JSNumber (JSRational r)) = return $ fromRational r
  readJSON _ = mkError "Unable to read Double"
    -- can't use JSRational here, due to ambiguous '0' parse
    -- it will parse as Integer.

instance JSON5 Float where
  showJSON = fromJSRational . toRational
  readJSON (JSNumber (JSRational r)) = return $ fromRational r
  readJSON (JSNumber (JSInfNaN n))   = return n
  readJSON _ = mkError "Unable to read Float"

-- -----------------------------------------------------------------
-- Sums

instance (JSON5 a) => JSON5 (Maybe a) where
  readJSON (JSObject o) = case "Just" `lookup` as of
      Just x -> Just <$> readJSON x
      _      -> case ("Nothing" `lookup` as) of
          Just JSNull -> return Nothing
          _           -> mkError "Unable to read Maybe"
    where as = fromJSObject o
  readJSON _ = mkError "Unable to read Maybe"
  showJSON (Just x) = JSObject $ toJSObject [("Just", showJSON x)]
  showJSON Nothing  = JSObject $ toJSObject [("Nothing", JSNull)]

instance (JSON5 a, JSON5 b) => JSON5 (Either a b) where
  readJSON (JSObject o) = case "Left" `lookup` as of
      Just a  -> Left <$> readJSON a
      Nothing -> case "Right" `lookup` as of
          Just b  -> Right <$> readJSON b
          Nothing -> mkError "Unable to read Either"
    where as = fromJSObject o
  readJSON _ = mkError "Unable to read Either"
  showJSON (Left a)  = JSObject $ toJSObject [("Left",  showJSON a)]
  showJSON (Right b) = JSObject $ toJSObject [("Right", showJSON b)]

-- -----------------------------------------------------------------
-- Products

instance JSON5 () where
  showJSON _ = JSArray []
  readJSON (JSArray []) = return ()
  readJSON _      = mkError "Unable to read ()"

instance (JSON5 a, JSON5 b) => JSON5 (a,b) where
  showJSON (a,b) = JSArray [ showJSON a, showJSON b ]
  readJSON (JSArray [a,b]) = (,) `fmap` readJSON a `ap` readJSON b
  readJSON _ = mkError "Unable to read Pair"

instance (JSON5 a, JSON5 b, JSON5 c) => JSON5 (a,b,c) where
  showJSON (a,b,c) = JSArray [ showJSON a, showJSON b, showJSON c ]
  readJSON (JSArray [a,b,c]) = (,,) `fmap`
                                  readJSON a `ap`
                                  readJSON b `ap`
                                  readJSON c
  readJSON _ = mkError "Unable to read Triple"

instance (JSON5 a, JSON5 b, JSON5 c, JSON5 d) => JSON5 (a,b,c,d) where
  showJSON (a,b,c,d) = JSArray [showJSON a, showJSON b, showJSON c, showJSON d]
  readJSON (JSArray [a,b,c,d]) = (,,,) `fmap`
                                  readJSON a `ap`
                                  readJSON b `ap`
                                  readJSON c `ap`
                                  readJSON d

  readJSON _ = mkError "Unable to read 4 tuple"

-- -----------------------------------------------------------------
-- List-like types


instance JSON5 a => JSON5 [a] where
  showJSON = showJSONs
  readJSON = readJSONs

-- container types:

#if !defined(MAP_AS_DICT)
instance (Ord a, JSON5 a, JSON5 b) => JSON5 (M.Map a b) where
  showJSON = encJSArray M.toList
  readJSON = decJSArray "Map" M.fromList

instance (JSON5 a) => JSON5 (IntMap.IntMap a) where
  showJSON = encJSArray IntMap.toList
  readJSON = decJSArray "IntMap" IntMap.fromList

#else
instance (Ord a, JSKey a, JSON5 b) => JSON5 (M.Map a b) where
  showJSON    = encJSDict . M.toList
  readJSON o  = M.fromList <$> decJSDict "Map" o

instance (JSON5 a) => JSON5 (IntMap.IntMap a) where
  {- alternate (dict) mapping: -}
  showJSON    = encJSDict . IntMap.toList
  readJSON o  = IntMap.fromList <$> decJSDict "IntMap" o
#endif


instance (Ord a, JSON5 a) => JSON5 (Set.Set a) where
  showJSON = encJSArray Set.toList
  readJSON = decJSArray "Set" Set.fromList

instance (Array.Ix i, JSON5 i, JSON5 e) => JSON5 (Array.Array i e) where
  showJSON = encJSArray Array.assocs
  readJSON = decJSArray "Array" arrayFromList

instance JSON5 I.IntSet where
  showJSON = encJSArray I.toList
  readJSON = decJSArray "IntSet" I.fromList

-- helper functions for array / object serializers:
arrayFromList :: (Array.Ix i) => [(i,e)] -> Array.Array i e
arrayFromList [] = Array.array undefined []
arrayFromList ls@((i,_):xs) = Array.array bnds ls
  where
  bnds = foldr step (i,i) xs

  step (ix,_) (mi,ma) =
    let mi1 = min ix mi
        ma1 = max ix ma
    in mi1 `seq` ma1 `seq` (mi1,ma1)


-- -----------------------------------------------------------------
-- ByteStrings

instance JSON5 S.ByteString where
  showJSON = encJSString S.unpack
  readJSON = decJSString "ByteString" (return . S.pack)

instance JSON5 L.ByteString where
  showJSON = encJSString L.unpack
  readJSON = decJSString "Lazy.ByteString" (return . L.pack)

-- -----------------------------------------------------------------
-- Data.Text

instance JSON5 T.Text where
  readJSON (JSString s) = return (T.pack . fromJSString $ s)
  readJSON _            = mkError "Unable to read JSString"
  showJSON              = JSString . toJSString . T.unpack


-- -----------------------------------------------------------------
-- Instance Helpers

makeObj :: [(String, JSValue)] -> JSValue
makeObj = JSObject . toJSObject

-- | Pull a value out of a JSON5 object.
valFromObj :: JSON5 a => String -> JSObject JSValue -> Result a
valFromObj k o = maybe (Error $ "valFromObj: Could not find key: " ++ show k)
                       readJSON
                       (lookup k (fromJSObject o))

encJSString :: (a -> String) -> a -> JSValue
encJSString f v = JSString (toJSString (f v))

decJSString :: String -> (String -> Result a) -> JSValue -> Result a
decJSString _ f (JSString s) = f (fromJSString s)
decJSString l _ _ = mkError ("readJSON{"++l++"}: unable to parse string value")

encJSArray :: (JSON5 a) => (b -> [a]) -> b -> JSValue
encJSArray f v = showJSON (f v)

decJSArray :: (JSON5 a) => String -> ([a] -> b) -> JSValue -> Result b
decJSArray _ f a@JSArray{} = f <$> readJSON a
decJSArray l _ _ = mkError ("readJSON{"++l++"}: unable to parse array value")

-- | Haskell types that can be used as keys in JSON5 objects.
class JSKey a where
  toJSKey   :: a -> String
  fromJSKey :: String -> Maybe a

instance JSKey JSString where
  toJSKey x   = fromJSString x
  fromJSKey x = Just (toJSString x)

instance JSKey Int where
  toJSKey   = show
  fromJSKey key = case reads key of
                    [(a,"")] -> Just a
                    _        -> Nothing

-- NOTE: This prevents us from making other instances for lists but,
-- our guess is that strings are used as keys more often then other list types.
instance JSKey String where
  toJSKey   = id
  fromJSKey = Just

-- | Encode an association list as 'JSObject' value.
encJSDict :: (JSKey a, JSON5 b) => [(a,b)] -> JSValue
encJSDict v = makeObj [ (toJSKey x, showJSON y) | (x,y) <- v ]

-- | Decode a 'JSObject' value into an association list.
decJSDict :: (JSKey a, JSON5 b)
          => String
          -> JSValue
          -> Result [(a,b)]
decJSDict l (JSObject o) = mapM rd (fromJSObject o)
  where rd (a,b) = case fromJSKey a of
                     Just pa -> readJSON b >>= \pb -> return (pa,pb)
                     Nothing -> mkError ("readJSON{" ++ l ++ "}:" ++
                                    "unable to read dict; invalid object key")

decJSDict l _ = mkError ("readJSON{"++ l ++ "}: unable to read dict; expected JSON5 object")


