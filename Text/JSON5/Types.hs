{-# LANGUAGE DeriveDataTypeable #-}
-- | The types to encode Haskell values.

module Text.JSON5.Types (

    -- * JSON5 Types
    JSValue(..)
  , JSNumber(..)
  , fromJSRational
  , fromJSInfNaN

    -- * Wrapper Types
  , JSString(..)
  , toJSString

  , JSObject(..)
  , toJSObject
  , get_field
  , set_field

  ) where

import Data.Typeable (Typeable)
import Data.String (IsString(..))

data JSValue
    = JSNull
    | JSBool     !Bool
    | JSNumber   JSNumber
    | JSString   JSString{-wrapped-}
    | JSArray    [JSValue]
    | JSObject   (JSObject JSValue)
    deriving (Eq, Ord, Show, Read, Typeable)

data JSNumber
    = JSRational !Rational
    | JSInfNaN   !Float
    deriving (Eq, Ord, Show, Read, Typeable)

fromJSRational :: Rational -> JSValue
fromJSRational = JSNumber . JSRational

fromJSInfNaN :: Float -> JSValue
fromJSInfNaN = JSNumber . JSInfNaN

newtype JSString = JSONString { fromJSString :: String }
    deriving (Eq, Ord, Show, Read, Typeable)

toJSString :: String -> JSString
toJSString = JSONString

instance IsString JSString where
  fromString = toJSString

instance IsString JSValue where
  fromString = JSString . fromString

newtype JSObject a = JSONObject { fromJSObject :: [(String, a)] }
    deriving (Eq, Ord, Show, Read, Typeable )

toJSObject :: [(String,a)] -> JSObject a
toJSObject = JSONObject

get_field :: JSObject a -> String -> Maybe a
get_field (JSONObject xs) x = lookup x xs

set_field :: JSObject a -> String -> a -> JSObject a
set_field (JSONObject xs) k v = JSONObject ((k,v) : filter ((/= k) . fst) xs)
