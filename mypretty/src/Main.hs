module Main (main) where

import System.Environment (getArgs)

import SimpleJSON
-- import PutJSON
import PrettyJSON (ppJValue, compactJValue)

json = (JObject (("myArrayValue", JArray ((JNumber 1):(JNumber 2):(JNumber 3):[])):("myNullValue", JNull):("myBoolValue", JBool True):[]))

-- main = putJValue json

main = do
    putStrLn "compact:"
    compactJValue json

    putStrLn "pretty:"
    width <- getIntArg
    ppJValue width json

getIntArg :: IO Int
getIntArg = fmap (read . head) getArgs
