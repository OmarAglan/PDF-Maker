{-DHUN| various helper functions DHUN-}
module Tools where
import Data.List (isPrefixOf, tails)
import Data.Char
import Data.List.Split (splitOn)
import Control.Monad
import System.IO
import System.IO.Strict
import Data.Time.Clock.POSIX

strip :: (Eq a) => [a] -> [a] -> [a]
strip l = reverse . (dropWhile isBad) . reverse . dropWhile isBad
  where isBad x = x `elem` l

{-DHUN| Pads a string with spaces. Adds space charactes to the beginning of a string until it has got a desired length. The first paramter is the desired length. The second paramter is the original string. The return value is the padded string. DHUN-}


pad :: Int -> String -> String
pad n s = if length s < n then pad n (' ' : s) else s

{-DHUN| creates a list of line numbers as padded strings. The first parameter is the length of length that each linenumber (a string) should have. The second paramter is the minimum linenumber to start with (inclusive). The third parameter is the maximum linenumber to end with (inclusive). The return value is the list of linenumbers as strings. DHUN-}

linenumbers :: Int -> Int -> Int -> [String]
linenumbers n mini maxi
  = if mini == maxi then [pad n (show mini)] else
      (pad n (show mini)) : (linenumbers n (mini + 1) maxi)

{-DHUN| Prints out a given message together with the current unix timestamp. The first parameter is the message to be printed DHUN-}

myprint :: String -> IO ()
myprint s
  = do t <- getPOSIXTime
       Prelude.putStrLn ("mediawiki2latex (" ++ (show t) ++ "):" ++ s)
       hFlush stdout

{-DHUN| Write a unicode string to a utf8 encoded file. The first parameter is the filename, the second the contend to be written to the file. DHUN-}

writeFile :: FilePath -> String -> IO ()
writeFile f s
  = do h <- openFile f WriteMode
       hSetEncoding h utf8
       hPutStr h s
       hClose h

{-DHUN| read a utf8 encoded file fully as a unicode string. The first parameter is the filename. The return value is the content of the file wrapped in the IO monad. DHUN-}

readFile :: FilePath -> IO String
readFile f
  = do h <- openFile f ReadMode
       hSetEncoding h utf8
       z <- System.IO.Strict.hGetContents h
       return z

{-DHUN| If the list is not empty it returns the list without the last item, otherwise the empty list- DHUN-}

nullinit :: [a] -> [a]
nullinit l = if null l then [] else init l

{-DHUN| Returns the list with the first element stripped wrapped in a Just of the Maybe monad. If the list is empty returns the value Nothing of the maybe monad. DHUN-}

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (_ : xs) = Just xs

{-DHUN| Returns the first element of a list wrapped in a Just of the Maybe monad. If the list is empty returns the value Nothing of the Maybe monad, DHUN-}

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

{-DHUN| The first parameter is and item. the second parameter is a list. If the list contains the item it returns the list up to the first occurrence of the item with the item itself excluded, otherwise it returns the empty list. DHUN-}

headSplitEq :: (Eq a) => a -> [a] -> [a]
headSplitEq c s
  = case splitOn [c] s of
        g : _ -> g
        [] -> []

{-DHUN| Removes all white space characters trailing on the right hand side of a string DHUN-}

rtrim :: String -> String
rtrim = reverse . (dropWhile isSpace) . reverse

{-DHUN| The first parameter is an original item. The second parameter is a replacement item. The third parameter is a list. This function replaces all occurrences of the original item, with the replacement item in the list. DHUN-}

replace :: (Eq a) => a -> a -> [a] -> [a]
replace src target = map (\ x -> if x == src then target else x)

{-DHUN| The first parameter is an input list. The second parameter is an original list that may or may not or may several times be part of the input list. The third parameter is an a replacement list. This function replaces all occurrences of the original list in the input list, with the replacement list. DHUN-}

replace2 :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace2 hay needle nail
  | needle `isPrefixOf` hay =
    nail ++ replace2 (drop (length needle) hay) needle nail
replace2 (x : xs) needle nail
  | otherwise = x : (replace2 xs needle nail)
replace2 [] _ _ = []

{-DHUN| The first parameter is an input list.  The second parameter is also a list, that might be contained in the input list. If this is the case this function returns true. Otherwise this function returns false  DHUN-}

isInfixOf2 :: (Eq a) => [a] -> [a] -> Bool
isInfixOf2 needle haystack
  = any (needle `isPrefixOf`) (tails haystack)

{-DHUN| Converts a single character in hex notation to an integer. The integer is wrapped in a mMybe monad. If an integer could be found it is wrapped in a Just of the Maybe monad. Otherwise the value Nothing of the Maybe monad is returned DHUN-}

unhexChar :: Char -> Maybe Integer
unhexChar c = lookup c hexTable
  where hexTable
          = zip ['0' .. '9'] [0 .. 9] ++
              zip ['a' .. 'f'] [10 .. 15] ++ zip ['A' .. 'F'] [10 .. 15]

{-DHUN| Converts a sequence of characters in hex notation to an integer. The integer is wrapped in a Maybe monad. If an integer could be found it is wrapped in a Just of the Maybe monad. Otherwise the value Nothing of the Maybe monad is returned DHUN-}

unhex :: String -> Maybe Integer
unhex = foldM f 0
  where f acc ch
          = maybe Nothing (\ val -> Just $ 16 * acc + val) (unhexChar ch)
