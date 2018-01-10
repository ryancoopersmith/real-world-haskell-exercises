module Main (main) where

import SimpleJSON

-- jsonTest = (JObject (("myArrayValue", JArray ((JNumber 1):(JNumber 2):(JNumber 3):[])):("myNullValue", JNull):("myBoolValue", JBool True):[]))
main = print (JObject [("foo", JNumber 1), ("bar", JBool False)])
