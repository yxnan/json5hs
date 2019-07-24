-- | Display JSON values using pretty printing combinators.

module Text.JSON5.Pretty
  ( ppJSValue

  , ppNull
  , ppBoolean

  , ppJSNumber
  , ppRational
  , ppInfNaN

  , ppArray
  , ppString
  , ppObject
  , ppJSString
  , ppJSObject
  ) where

import Text.JSON5.Types
import Text.PrettyPrint.HughesPJ
import qualified Text.PrettyPrint.HughesPJ as PP
import Data.Ratio
import Data.Char
import Numeric

ppJSValue        :: JSValue -> Doc
ppJSValue v       = case v of
    JSNull       -> ppNull
    JSBool x     -> ppBoolean x
    JSNumber jsn -> ppJSNumber jsn
    JSString x   -> ppJSString x
    JSArray vs   -> ppArray vs
    JSObject xs  -> ppJSObject xs

ppNull :: Doc
ppNull = text "null"

ppBoolean :: Bool -> Doc
ppBoolean True  = text "true"
ppBoolean False = text "false"

ppJSNumber :: JSNumber -> Doc
ppJSNumber (JSRational r) = ppRational r
ppJSNumber (JSInfNaN n)   = ppInfNaN n

ppRational :: Rational -> Doc
ppRational x
  | denominator x == 1 = integer (numerator x)
  | otherwise          = double (fromRational x)

ppInfNaN :: Float -> Doc
ppInfNaN n
  | isNaN n = text "NaN"
  | n > 0   = text "Infinity"
  | n < 0   = text "-Infinity"

ppArray :: [JSValue] -> Doc
ppArray xs = brackets $ fsep $ punctuate comma $ map ppJSValue xs

ppString :: String -> Doc
ppString x = doubleQuotes $ hcat $ map pp_char x
  where pp_char '\\'            = text "\\\\"
        pp_char '"'             = text "\\\""
        pp_char c | isControl c = uni_esc c
        pp_char c               = char c

        uni_esc c = text "\\u" PP.<> text (pad 4 (showHex (fromEnum c) ""))

        pad n cs  | len < n   = replicate (n-len) '0' ++ cs
                  | otherwise = cs
          where len = length cs

ppObject :: [(String,JSValue)] -> Doc
ppObject xs = braces $ fsep $ punctuate comma $ map pp_field xs
  where pp_field (k,v) = ppString k PP.<> colon <+> ppJSValue v

ppJSString :: JSString -> Doc
ppJSString = ppString . fromJSString

ppJSObject :: JSObject JSValue -> Doc
ppJSObject = ppObject . fromJSObject
