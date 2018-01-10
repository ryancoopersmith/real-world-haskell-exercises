module Main (main) where

import SimpleJSON

main = print (JObject (("myArrayValue", JArray ((JNumber 1):(JNumber 2):(JNumber 3):[])):("myNullValue", JNull):("myBoolValue", JBool True):[]))
