module Main (main) where

import SimpleJSON
-- import PutJSON
import PrettyJSON (ppJValue, compactJValue)

json = (JObject (("myArrayValue", JArray ((JNumber 1):(JNumber 2):(JNumber 3):[])):("myNullValue", JNull):("myBoolValue", JBool True):[]))

-- main = putJValue json
main = ppJValue 80 json
-- main = compactJValue json
