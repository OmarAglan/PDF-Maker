-- | various helper functions
module Tools where

import Control.Monad
import Data.Char
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Time.Clock.POSIX
import System.IO
import System.IO.Strict

-- | Removes superfluous heading and tailing characters from a string.
strip ::
  (Eq a) =>
  -- | the list of characters which are considered superfluous.
  [a] ->
  -- | the initial string from which the superfluous characters shall be
  -- removed.
  [a] ->
  -- | the output string in which the superfluous characters have been removed
  -- from the beginning and the end of the original string.
  [a]
strip l = reverse . (dropWhile isBad) . reverse . dropWhile isBad
  where
    isBad x = x `elem` l

-- | Pads a string with spaces. Adds space characters to the beginning of a
-- string until it has got a desired length.
pad ::
  -- | The desired length for the result string
  Int ->
  -- | The input string.
  String ->
  -- | The padded string as output of this function-
  String
pad n s = if length s < n then pad n (' ' : s) else s

-- | Creates a list of line numbers as padded strings.
linenumbers ::
  -- | The length of length that padded decimal character representation of each
  -- line number should have
  Int ->
  -- | The minimum line number to start with (inclusive).
  Int ->
  -- | The maximum line number to end with (inclusive).
  Int ->
  -- | The list of line numbers as strings.
  [String]
linenumbers n mini maxi =
  if mini == maxi
    then [pad n (show mini)]
    else (pad n (show mini)) : (linenumbers n (mini + 1) maxi)

-- | Prints out a given message together with the current Unix timestamp. Used
-- everywhere in mediawiki2latex in order to display status messages.
myprint ::
  -- | The message to be printed
  String ->
  -- | The IO action to print the given string together with the current Unix
  -- timestamp.
  IO ()
myprint s =
  do
    t <- getPOSIXTime
    Prelude.putStrLn ("mediawiki2latex (" ++ (show t) ++ "):" ++ s)
    hFlush stdout

-- | Writes a Unicode string to a UTF8 encoded file.
writeFile ::
  -- | The filename of the file where the string given second parameter shall be
  -- written.
  FilePath ->
  -- | The contend (a string) to be written to the file with the filename given
  -- in the first parameter.
  String ->
  -- | The IO action writing the string to the file.
  IO ()
writeFile f s =
  do
    h <- openFile f WriteMode
    hSetEncoding h utf8
    hPutStr h s
    hClose h

-- | Reads a UTF8 encoded file fully as a Unicode string.
readFile ::
  -- |  The filename of the file from which the string shall be red.
  FilePath ->
  -- | The content of the file wrapped in the IO monad.
  IO String
readFile f =
  do
    h <- openFile f ReadMode
    hSetEncoding h utf8
    z <- System.IO.Strict.hGetContents h
    return z

-- | If the input list is not empty it returns the input list without the last
-- item, otherwise the empty list.
nullinit ::
  -- | The input list
  [a] ->
  -- | The input list with the last item removed if the input list is not the
  -- empty list. Otherwise the empty list.
  [a]
nullinit l = if null l then [] else init l

-- | Returns the list with the first element stripped wrapped in a Just of the
-- Maybe monad. If the list is empty returns the value Nothing of the maybe
-- monad.
maybeTail ::
  -- | The input list
  [a] ->
  -- | The input list without the first element in a Just value of the Maybe
  -- monad if the input list if not the empty list. Otherwise the Nothing value
  -- of the maybe monad is returned.
  Maybe [a]
maybeTail [] = Nothing
maybeTail (_ : xs) = Just xs

-- | Returns the first element of a list wrapped in a Just of the Maybe monad.
-- If the list is empty returns the value Nothing of the Maybe monad.
maybeHead ::
  -- | The input list
  [a] ->
  -- | The first element of the input list wrapped in a Just value of the Maybe
  -- monad if the input list is not the empty list. Otherwise the Nothing value
  -- of the Maybe monad.
  Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

-- | The first parameter is an item. the second parameter is a list. If the list
-- contains the item it returns the list up to the first occurrence of the item
-- with the item itself excluded, otherwise it returns the empty list.
headSplitEq ::
  (Eq a) =>
  -- | The item at up to which the list given in the second parameter shall be
  -- returned (excluding the item itself)
  a ->
  -- | The list of which the first part up to the item given in the first
  -- parameter shall be returned. The item given in the first parameter itself
  -- shall be excluded.
  [a] ->
  -- | The first part of list given as second input parameter up the item given
  -- as first parameter if the list contains the item given as the first
  -- parameter. Otherwise the empty list. The item given as the first parameter
  -- itself is not part of the return value in any case.
  [a]
headSplitEq c s =
  case splitOn [c] s of
    g : _ -> g
    [] -> []

-- | Removes all white space characters trailing on the right hand side of a
-- string
rtrim ::
  -- | the input string of which the white space character on the right hand
  -- side shall be removed.
  String ->
  -- | the input string given as first parameter with the any white space
  -- character on the right hand side removed.
  String
rtrim = reverse . (dropWhile isSpace) . reverse

-- | This function replaces all occurrences of the original item, with the
-- replacement item in the list.
replace ::
  (Eq a) =>
  -- | an original item to be replaced
  a ->
  -- | the replacement item the original item shall be replaced with.
  a ->
  -- | the list in which all original items shall be replaced with the
  -- replacement item
  [a] ->
  -- | the output list in which all occurrences of original items have been
  -- replaced with the replacement item.
  [a]
replace src target = map (\x -> if x == src then target else x)

-- | This function replaces all occurrences of the original list in the input
-- list, with the replacement list. It replaces all needles in the haystack with
-- nails.
replace2 ::
  (Eq a) =>
  -- | the original input (haystack) list which may contain any number
  -- (including zero) of the sublist (the needle) to be replaced.
  [a] ->
  -- | the sublist (the needle) which may be contained in the original list (the
  -- haystack) and number of times.
  [a] ->
  -- | the list (the nail) with which the sublist (the needle) shall be replaced
  -- in the original list (the haystack)
  [a] ->
  -- | the output list in which is the input list (the haystack) where all
  -- occurrences of the input sublist (the needle) have been replaced with the
  -- output list (the nail)
  [a]
replace2 hay needle nail
  | needle `isPrefixOf` hay =
      nail ++ replace2 (drop (length needle) hay) needle nail
replace2 (x : xs) needle nail
  | otherwise = x : (replace2 xs needle nail)
replace2 [] _ _ = []

-- | Converts a single character in hex notation to an integer. The integer is
-- wrapped in a Maybe monad. If an integer could be found it is wrapped in a
-- Just value of the Maybe monad. Otherwise the value Nothing of the Maybe monad
-- is returned
unhexChar ::
  -- | the char to be converted for hexadecimal notation the integer.
  Char ->
  -- | the converted integer in a Just value of the Maybe monad if the
  -- conversion was successful, otherwise the Nothing value of the Maybe monad.
  Maybe Integer
unhexChar c = lookup c hexTable
  where
    hexTable =
      zip ['0' .. '9'] [0 .. 9]
        ++ zip ['a' .. 'f'] [10 .. 15]
        ++ zip ['A' .. 'F'] [10 .. 15]

-- | Converts a sequence of characters in hex notation to an integer. The
-- integer is wrapped in a Maybe monad. If an integer could be found it is
-- wrapped in a Just of the Maybe monad. Otherwise the value Nothing of the
-- Maybe monad is returned
unhex ::
  -- | the hexadecimal string to be converted to an integer.
  String ->
  -- | the converted integer wrapped in a Just value of the Maybe monad if the
  -- conversion was successful, otherwise the Nothing value of the Maybe monad.
  Maybe Integer
unhex = foldM f 0
  where
    f acc ch =
      maybe Nothing (\val -> Just $ 16 * acc + val) (unhexChar ch)
