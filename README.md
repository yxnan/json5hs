<div align="center">
<h1>json5hs: Serialising to and from JSON5</h1>
<p>
      <a href="https://hackage.haskell.org/package/json5hs"><img src="https://img.shields.io/hackage/v/json5hs?color=blue" alt="Hackage"></a>
      <a href="https://matrix.hackage.haskell.org/#/package/json5hs"><img src="https://img.shields.io/badge/Hackage%20CI-avaliable-brightgreen" alt="Build"></a>
</p>
</div>

---

This library provides a parser and pretty printer for converting
between Haskell values and JSON5.

## Example

```haskell
ghci> import Text.JSON5
ghci> encode [("key1",1),("key2",2)]
"[[\"key1\",1],[\"key2\",2]]"

ghci> import Text.JSON5.String (runGetJSON)
ghci> input <- getLine 
{'singleQuotes': 0xabcde, pos: +3, infnan: +Infinity, escape: "\t\u1234", trailing-comma: ['here',], }
ghci> runGetJSON readJSValue input
Right (JSObject (JSONObject {fromJSObject = [("singleQuotes",JSNumber (JSRational (703710 % 1))),("pos",JSNumber (JSRational (3 % 1))),("infnan",JSNumber (JSInfNaN Infinity)),("escape",JSString (JSONString {fromJSString = "\t\4660"})),("trailing-comma",JSArray [JSString (JSONString {fromJSString = "here"})])]}))

ghci> import Text.JSON5.Pretty
ghci> ppJSValue $ makeObj [("key1", JSString $ toJSString "value1"), ("key2", JSArray [JSNull, JSBool True])]
{"key1": "value1", "key2": [null, true]}

ghci> import Text.JSON5.Generic
ghci> ppJSValue $ toJSON $ Just [2,1,4]
{"Just": [2, 1, 4]}
ghci> fromJSON (JSString $ toJSString "string") :: Result String 
Ok "string"
```

