-- | Module to handle the typesetting of chessboards
module Wiki2Fen where

import Data.Char
import Data.List hiding (drop, lookup, take)
import Data.List.Split
import Data.Map.Strict hiding (drop, foldr, map, take)
import Tools (strip)

-- | mapping of integers to their respective chessboard column indices.
letters ::
  -- | Take an integer to its respective chessboard column index.
  Map Integer Char
letters = fromList (zip [0 ..] ['a' .. 'h'])

-- | Reads a single chess square in wiki notation. Outputs a Nothing value of
-- the Maybe monad if the square is marked. Output a Just value containing the
-- empty list if the square is empty. Outputs a Just value contain a list which
-- contains a single character in case there is chess piece on the square. This
-- character is the symbol representing the chess piece in FEN chess notation.
bf ::
  -- | The contents of the chess square in wiki notation.
  String ->
  -- | The parsed version of the chess square partly to be converted in FEN
  -- chess notation.
  Maybe String
bf (x : 'd' : []) | pre x = Just [toLower x]
bf (x : 'l' : []) | pre x = Just [toUpper x]
bf ('x' : 'x' : []) = Nothing
bf _ = Just []

-- | A predicate returning true if and only if the given character represents a
-- dark chess piece in FEN chess notation.
pre ::
  -- | The character to be checked
  Char ->
  -- | True if the character to be checked represents a dark chess piece in FEN
  -- chess notation.
  Bool
pre x = x `elem` "rnbqkp"

-- | Convert a row in parsed notation to a row into highlighting notation of the
-- chessboard LaTeX package.
fun ::
  -- | A pair. The first element of it is the row index. The second element of
  -- it is a list of pairs. The first element of each pair in the list is the
  -- column index. The second element each pair in the list is the parsed
  -- notation of the content of the chess square as returned by the `bf`
  -- function.
  (Integer, [(Integer, Maybe String)]) ->
  -- | Temporary accumulator. Should be the empty list when called externally.
  [String] ->
  -- | The row in the highlighting notation of the chessboard LaTeX package.
  [String]
fun (r, x) acu = acu ++ (foldr (fun2 r) [] (reverse x))

-- | Convert a single chess square from the parsed notation to the highlighting
-- notation of the chessboard LaTeX package.
fun2 ::
  -- | The row index.
  Integer ->
  -- | A pair. The first element is the column index. The second element is the
  -- parsed version of the content of the chess square as returned by the `bf`
  -- function.
  (Integer, Maybe String) ->
  -- | Temporary accumulator. Should be the empty list when called externally.
  [String] ->
  -- | The chess square in the highlighting notation of the chessboard LaTeX
  -- package.
  [String]
fun2 r (c, Nothing) acu =
  acu
    ++ case Data.Map.Strict.lookup c letters of
      Just z -> [[z] ++ (show (8 - r))]
      _ -> []
fun2 _ _ acu = acu

-- | Convert a row in parsed notation to a row into FEN chess notation.
fun3 ::
  -- | A pair. The first element of it is the row index. The second element of
  -- it is a list of pairs. The first element of each pair in the list is the
  -- column index. The second element each pair in the list is the parsed
  -- notation of the content of the chess square as returned by the `bf`
  -- function.
  (Integer, [(Integer, Maybe String)]) ->
  -- | Temporary accumulator. Should be the empty list when called externally.
  [String] ->
  -- | The row in the FEN chess notation.
  [String]
fun3 (r, x) acu = acu ++ [a ++ (if i > 0 then (show i) else "")]
  where
    (a, i) = (foldr (fun4 r) ([], 0) (reverse x))

-- | Convert a single chess square from the parsed notation to the FEN chess
-- notation.
fun4 ::
  -- | The row index
  Integer ->
  -- | A pair. The first element is the column index. The second element is the
  -- parsed version of the content of the chess square as returned by the `bf`
  -- function.
  (Integer, Maybe String) ->
  -- | A pair. The first element accumulates the String in FEN chess notation.
  -- The second element is an integer that tracks the number of empty chess
  -- squares since the last addition to the accumulating first element of the
  -- tuple.
  (String, Integer) ->
  -- | A pair. The first element is the row in FEN chess notation without
  -- possible tailing empty squares. The number of empty squares following in
  -- the end of the row
  (String, Integer)
-- \^ A pair. The first element is a String representing the row in FEN chess
-- notation without possibly tailing empty squares. The second element is an
-- Integer representing the number of empty squares in the end of the row.
fun4 _ (_, Just "") (acu, i) = (acu, i + 1)
fun4 _ (_, Just x) (acu, i) = (acu ++ (if i > 0 then (show i) else "") ++ x, 0)
fun4 _ _ (acu, i) = (acu, i + 1)

-- | Convert a wiki chessboard notation to FEN chess notation and the
-- highlighting notation LaTeX chessboard package.
toBoard ::
  -- | The chessboard in Wiki notation
  String ->
  -- | A pair, the first element is the chessboard in FEN chess notation. The
  -- second element is the chessboard in highlighting notation of the LaTeX
  -- chessboard package.
  (String, String)
toBoard x = ((intercalate "/" (foldr fun3 [] (reverse l))) ++ " b - - 0 1", intercalate "," ((foldr fun [] (reverse l)) :: [String]))
  where
    sp = splitOn "|=" x
    l = zip [0 ..] (map (\y -> zip [0 ..] (map (bf . (strip "\n\r\t ")) ((drop 1) ((splitOn "|") y)))) (take 8 sp))
