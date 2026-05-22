-- | module to convert string to hexadecimally encodes strings and vice versa
module Hex where

import Data.Char
import Data.Map.Strict hiding (map)
import Data.Maybe

-- | list of integers in the range from 0 to 15. So one hex digit.
nums ::
  -- | the list of integers in the range from 0 to 15
  [Int]
nums = [0 .. 15]

-- | list of single digit hex numbers in ascending order
chars ::
  -- | the list of characters representing a hex digit
  [Char]
chars = (['0' .. '9'] ++ ['A' .. 'F'])

-- | map from integer to hex digit
fromm ::
  -- | A map from integers to their hex digit representation for all integer in
  -- the range of one hex digit.
  Map Int Char
fromm = fromList $ zip nums chars

-- | map from a hex digit to integer
tom ::
  -- | A map form a hex single hex digits to their integer representations
  Map Char Int
tom = fromList $ zip chars nums

-- | function to convert a single Unicode character (Char) to a hex encodes
-- string. Each character is converted to eight hexadecimal digits. Leading
-- zeros are a added if need to fill the full eight hex digits.
hexChar ::
  -- | The Unicode character to be converter to hexadecimal notation
  Char ->
  -- | Returns the hexadecimal representation of the Unicode character
  String
hexChar c =
  concat
    ( map
        ( \i ->
            fromMaybe
              ""
              ( ( Data.Map.Strict.lookup
                    ( (if (i == 0) then (ord c) else (ord c) `div` (16 ^ i))
                        `mod` (16 :: Int)
                    )
                    fromm
                )
                  >>= (\x -> return [x])
              )
        )
        (reverse [0 .. 7] :: [Int])
    )

-- | function to convert a string of Unicode characters to a hex encoded version
-- of it. Each Unicode character is represented by 8 hex digits.
hex ::
  -- | The string of Unicode characters to be converted to their hexadecimal
  -- representation
  String ->
  -- | The concatenation of the hexadecimal representations of the Unicode
  -- characters given in the first Parameter.
  String
hex s = concat (map hexChar s)

-- | function to decode a hex encoded Unicode string. Groups of eight hex digit
-- are converted to a single Unicode characters each.
unhex ::
  -- | The hexadecimal representation of the Unicode string (eight digits per
  -- character).
  String ->
  -- | The unicode String converted from the hexadecimal representation given as
  -- first parameter.
  String
unhex (a : (b : (c : (d : (e : (f : (g : (h : xs)))))))) =
  ( chr
      ( sum
          ( map
              ( \(i, cc) ->
                  (16 ^ i) * (fromMaybe 0 (Data.Map.Strict.lookup cc tom))
              )
              (zip (reverse [0 .. 7] :: [Int]) [a, b, c, d, e, f, g, h])
          )
      )
  )
    : (unhex xs)
unhex _ = []
