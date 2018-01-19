module Logger (
    Logger,
    Log,
    runLogger,
    record
) where

globToRegex :: String -> Logger String
globToRegex cs =
    globToRegex' cs >>= \ds ->
    return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"

globToRegex' ('?':cs) =
    record "any" >>
    globToRegex' cs >>= \ds ->
    return ('.':ds)

{- the above and below function bodies are both valid and fundamentally the same
 - but the below one with the do notation is much cleaner -}

globToRegex' ('*':cs) = do
    record "kleene star"
    ds <- globToRegex' cs
    return (".*" ++ ds)

type Log = [String]

runLogger :: Logger a -> (a, Log)

record :: String -> Logger ()
