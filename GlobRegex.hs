module GlobRegex (
    globToRegex,
    matchesGlob
) where

import Text.Regex.Posix

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""

globToRegex' ('*':cs) = ".*" ++ globToRegex' cs

globToRegex' ('?':cs) = '.' : globToRegex' cs

globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs)     = '['  :  c : charClass cs
globToRegex' ('[':_)        = error "unterminated character class"

globToRegex' (c:cs) = escape c ++ globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"

matchesGlob :: Bool -> String -> FilePath -> Bool
matchesGlob ins pat name = if ins
                           then name =~+ (globToRegex pat, compIgnoreCase, execBlank)
                           else name =~ globToRegex pat

-- my case insensitive regex matcher function
(=~+) :: (RegexMaker regex compOpt execOpt source,
          RegexContext regex source1 target)
         => source1 -> (source, compOpt, execOpt) -> target

source1 =~+ (source, compOpt, execOpt)
        = match (makeRegexOpts compOpt execOpt source) source1
