{-DHUN| module to convert string to hexadecimally endoces strings and vice versa DHUN-}
module Hex where
import Data.Char
import Data.Map.Strict hiding (map)
import Data.Maybe

{-DHUN| list of integer in the range from 0 to 15. So one hex digit DHUN-}

nums :: [Int]
nums = [0 .. 15]

{-DHUN| list of single digit hex numbers in ascending order DHUN-}

chars :: [Char]
chars = (['0' .. '9'] ++ ['A' .. 'F'])

{-DHUN| map from integer to hex digit DHUN-}

fromm :: Map Int Char
fromm = fromList $ zip nums chars

{-DHUN| map from hex digit to integer DHUN-}

tom :: Map Char Int
tom = fromList $ zip chars nums

{-DHUN| function to convert a single unicode character (Char) to a hex encodes string DHUN-}

hexChar :: Char -> String
hexChar c
  = concat
      (map
         (\ i ->
            fromMaybe ""
              ((Data.Map.Strict.lookup
                  ((if (i == 0) then (ord c) else (ord c) `div` (16 ^ i)) `mod`
                     (16 :: Int))
                  fromm)
                 >>= (\ x -> return [x])))
         (reverse [0 .. 7] :: [Int]))

{-DHUN| function to convert a string of unicode characters to a hex encoded version of it DHUN-}

hex :: String -> String
hex s = concat (map hexChar s)

{-DHUN| function to decode a hex encoded unicode string DHUN-}

unhex :: String -> String
unhex (a : (b : (c : (d : (e : (f : (g : (h : xs))))))))
  = (chr
       (sum
          (map
             (\ (i, cc) ->
                (16 ^ i) * (fromMaybe 0 (Data.Map.Strict.lookup cc tom)))
             (zip (reverse [0 .. 7] :: [Int]) [a, b, c, d, e, f, g, h]))))
      : (unhex xs)
unhex _ = []
