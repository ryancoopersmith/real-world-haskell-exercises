{- This Logger monad is a specialised version of the standard Writer monad,
 - which can be found in the Control.Monad.Writer module of the mtl package -}

module Logger (
    Logger,
    Log,
    runLogger,
    record
) where

newtype Logger a = Logger { execLogger :: (a, Log) }

type Log = [String]

{- Applicative Monad Proposal (AMP) requires Monads
 - to also be instances of Applicative -}

{- fmap and liftM are the same, and we know logger is a Monad
 - so we can define fmap in terms of liftM -}
instance Functor Logger where
  fmap = liftM

instance Applicative Logger where
  pure a = Logger (a, [])
  Logger (f, _) <*> Logger (a, _) = pure (f a)

{- writting return in terms of pure as suggested by the AMP
 - (https://wiki.haskell.org/Functor-Applicative-Monad_Proposal) -}
instance Monad Logger where
    return = pure

    -- (>>=) :: Logger a -> (a -> Logger b) -> Logger b
    m >>= k = let (a, w) = execLogger m
                  n      = k a
                  (b, x) = execLogger n
              in Logger (b, w ++ x)

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

-- predefined in Control.Monad
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= \i ->
            return (f i)

-- also in Control.Monad
liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 =
    m1 >>= \a ->
    m2 >>= \b ->
    return (f a b)

escape :: Char -> Logger String
escape c
    | c `elem` regexChars = record "escape" >> return ['\\',c]
    | otherwise           = return [c]
  where regexChars = "\\+()^$.{}]|"

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

-- the above and below function bodies are both valid and fundamentally the same

globToRegex' ('*':cs) = do
    record "kleene star"
    ds <- globToRegex' cs
    return (".*" ++ ds)

globToRegex' ('[':'!':c:cs) =
    record "character class, negative" >>
    charClass cs >>= \ds ->
    return ("[^" ++ c : ds)

globToRegex' ('[':c:cs) =
    record "character class" >>
    charClass cs >>= \ds ->
    return ("[" ++ c : ds)

globToRegex' ('[':_) =
    fail "unterminated character class"

globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

-- w/o liftM
charClassWordy (']':cs) =
    globToRegex' cs >>= \ds ->
    return (']':ds)
charClassWordy (c:cs) =
    charClassWordy cs >>= \ds ->
    return (c:ds)

-- w/ liftM (much cleaner)
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs
