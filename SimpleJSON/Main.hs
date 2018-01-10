module Main (main) where

import SimpleJSON
import PutJSON

main = putJValue (JObject (("myArrayValue", JArray ((JNumber 1):(JNumber 2):(JNumber 3):[])):("myNullValue", JNull):("myBoolValue", JBool True):[]))
