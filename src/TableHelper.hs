-- | Helper functions to convert tables from the parse tree notation to the
-- latex notation
module TableHelper where

import Control.Monad
import Data.Char
import Data.List (intercalate, isInfixOf)
import qualified Data.Map as Map
import Data.Map.Strict (Map, keys)
import Data.Maybe
import MediaWikiParseTree
import MyState
import Text.Printf

-- | The width of a columns as float wrapped in a Just value of the maybe monad
-- if it could be determined. Return the value Nothing of the maybe monad
-- otherwise. The only parameter is of part of the parse tree that describe the
-- opening part of the table cell element.
widthOfColumn ::
  -- | the inner part of the table cell element as parse tree representation.
  [Anything Char] ->
  -- | the fraction of the width of the width of the whole tables spanned by the
  -- current column.
  Maybe Float
widthOfColumn = msum . (map f)
  where
    f (Environment Attribute (Attr (k, v)) _) =
      listToMaybe $
        do
          (z, _) <- reads v
          guard (k == "width")
          guard ('%' `elem` v)
          return (1.0e-2 * z)
    f _ = Nothing

-- | The column multiplicity for calculation of the `raggedArrayOfWidths`. It is
-- -1 if the current column has not column multiplicity otherwise it is the
-- number found in the colspan attribute of the parsetree part given as the
-- first parameter
columnMultiplicityForSimple ::
  -- | the parse tree representation of the inner part of table cell element
  -- (e.g. th or td HTML tag, or their wiki equivalent).
  [Anything Char] ->
  -- | the content of the colspan attribute if found, otherwise -1.
  Int
columnMultiplicityForSimple x =
  case columnMultiplicity x of
    Just a -> a
    _ -> -1

-- | Returns a ragged array of column width calculated from parse tree
-- representation of the inner part of a table. It returns a list of lists. The
-- inner lists are the rows of the table. If a cell in a row has a known width
-- it is represented by a Just value of the Maybe monad containing its width as
-- a fraction of the total width of the column, otherwise it is represented by
-- the Nothing value of the Maybe monad. If a cell spans multiple columns some
-- Nothing values are inserted after the cell. The number of the Nothing values
-- is the column span of the cell minus one.
raggedArrayOfWidths ::
  -- | The parse tree representation of the inner part of the table
  [Anything Char] ->
  -- | accumulating variable. Has to be the empty list when calling this
  -- function form outside.
  [Maybe Float] ->
  -- | the ragged array of column widths.
  [[Maybe Float]]
raggedArrayOfWidths ((Environment TableRowSep _ _) : xs) temp =
  if temp == []
    then raggedArrayOfWidths xs []
    else temp : (raggedArrayOfWidths xs [])
raggedArrayOfWidths ((Environment TableColSep _ x) : xs) temp =
  ( raggedArrayOfWidths
      xs
      ( temp
          ++ [widthOfColumn x]
          ++ ( concat
                 (replicate ((columnMultiplicityForSimple x) - 1) [Nothing])
             )
      )
  )
raggedArrayOfWidths ((Environment TableHeadColSep _ x) : xs) temp =
  ( raggedArrayOfWidths
      xs
      ( temp
          ++ [widthOfColumn x]
          ++ ( concat
                 (replicate ((columnMultiplicityForSimple x) - 1) [Nothing])
             )
      )
  )
raggedArrayOfWidths (_ : xs) temp = (raggedArrayOfWidths xs temp)
raggedArrayOfWidths [] temp = [temp]

-- | Returns the number of columns in a table.
numberOfColumns ::
  -- | the parse tree representation of the inner parse of table.
  [Anything Char] ->
  -- | the number of columns in the table. Cell with a colspan attribute are
  -- counted accordingly.
  Int
numberOfColumns a =
  (maximum ([length x | x <- (raggedArrayOfWidths a [])]))

-- | Create a list of Nothing values of the Maybe monad, which has got a length
-- of the number of columns of the parse tree representation of the inner part
-- of a table given as the first parameter. The result stands for a list of
-- table column widths, where the width of each column is unknown (hence the
-- Nothing values).
initialListofWidths ::
  -- | The parse tree representation of an inner part of a table
  [Anything Char] ->
  -- | A list of widths of columns where each column has an unknown width.
  [Maybe Float]
initialListofWidths x = replicate (numberOfColumns x) Nothing

-- | This function takes two lists of widths of columns and returns the maximum
-- of both again as a list of width of column. An Nothing value of the Maybe
-- monad in this list is interpreted as a column with unknown width. So any Just
-- value is considered greater than any Nothing value. If two just values are
-- compared the one with the higher value inside the Just is used. If one list
-- is too short, it is handled as if it was long enough but contained only
-- Nothing values on any position after it ended.
listMax ::
  -- | A list of widths of columns.
  [Maybe Float] ->
  -- | Another list of width of columns .
  [Maybe Float] ->
  -- | The maximum of the two lists of columns.
  [Maybe Float]
listMax (Just x : xs) (Just y : ys) =
  Just (max x y) : listMax xs ys
listMax (x : xs) (y : ys) = (x `mplus` y) : listMax xs ys
listMax [] (y : ys) = y : listMax [] ys
listMax (x : xs) [] = x : listMax xs []
listMax [] [] = []

-- | Calculates the preliminary width of columns from the `raggedArrayOfWidths`
-- given as first parameter and the `initialListofWidths` given in the second
-- parameter. See also `listMax` for how the calculating works.
preliminaryWidths ::
  -- | The `raggedArrayOfWidths`
  [[Maybe Float]] ->
  -- | The `initialListofWidths`
  [Maybe Float] ->
  -- | A list of width of columns
  [Maybe Float]
preliminaryWidths l k = foldl (listMax) k l

-- | Calculates the default width of column for which no width is explicitly
-- defined. Return the Nothing value of the Maybe monad if it could not be
-- determined. Otherwise it return the result wrapped in a Just value of the
-- Maybe monad.
standardColumnWitdh ::
  -- | The parse tree representation of the inner part of table
  [Anything Char] ->
  -- | The default width of columns which don't have an explicitly defined
  -- width.
  Maybe Float
standardColumnWitdh a =
  if columns > columnsWithDefinedWidth
    then
      if sumOfDefinedWidths > 1.0
        then Nothing
        else
          Just $
            (1.0 - sumOfDefinedWidths)
              / fromIntegral (columns - columnsWithDefinedWidth)
    else if sumOfDefinedWidths > 1.001 then Nothing else Just 0.0
  where
    l = rawWidths a
    columns = numberOfColumns a
    columnsWithDefinedWidth = length (filter isJust l)
    sumOfDefinedWidths = sum (map fromJust (filter isJust l))

-- | It takes the parse tree representation of the inner part of a table and
-- returns a list of widths of columns. It contains Just values of the Maybe
-- monad for each column which has an explicitly defined with and a Nothing
-- value of the Maybe monad for the column which don't have an explicitly
-- defined width.
rawWidths ::
  -- | The parse tree representation of the inner part of a table.
  [Anything Char] ->
  -- | The list of the widths of the columns in the table.
  [Maybe Float]
rawWidths a =
  ( preliminaryWidths
      (raggedArrayOfWidths a [])
      (initialListofWidths a)
  )

-- | Fallback function for the width of columns if the precompilation procedure
-- for the width of columns is not available yet. So particularly when the
-- precompilation is just running. Takes the parse tree representation of the
-- table as first input parameter. Returns the list of widths of the columns.
-- The final width of the columns don't have anything to do with the width
-- calculated here, still these preliminary widths are needed for the
-- precompilation procedure.
columnWidths ::
  -- | The parse tree representation of the inner part of a table
  [Anything Char] ->
  -- | the widths of the columns of the table given in the first parameter
  [Float]
columnWidths a = w
  where
    l = rawWidths a
    m = numberOfColumns a
    mf = fromIntegral m
    f = 1.0 - (scalefactor mf)
    w = fromMaybe (concat (replicate m [f / mf])) $
      do
        ww <- standardColumnWitdh a
        return [x * f | x <- map (fromMaybe ww) l]

-- | Part of the correction calculation for the space between columns inside a
-- table. Takes the number of columns as first input parameter returns a scaling
-- factor. Scalefactor must not be 1.0 or higher to avoid stack overflow error
-- by infinite loop in LaTeXRenderer, therefore limiting it to 34 columns per
-- table
scalefactor ::
  (Fractional a, Ord a) =>
  -- | The number of columns in the table.
  a ->
  -- | The scalefactor. That is the part of the space of the table needed for
  -- vertical separation between the columns.
  a
scalefactor n | n <= 34 = 12.8 * (n) / 448.0
scalefactor _ = 12.8 * (34.0) / 448.0

-- | Part of the correction calculation for the space between columns inside a
-- table. Takes the number of columns as first input parameter returns a scaling
-- factor. That is roughly the amount of space available for a single cell as a
-- fraction of the linewidth
tableScale ::
  -- | The number of columns in the table.
  Int ->
  -- | The scaling factor.
  Float
tableScale nColumns = (1.0 / n) * (1.0 - (scalefactor n))
  where
    n = fromIntegral nColumns

-- | Returns the latex environment name for a table environments. If the float
-- passes as first parameter it 1.0 the result is longtable, otherwise it is
-- tabular. This function is usually called with the width of the current cell
-- in units of the line width as first parameter. Outside any table this value
-- is 1.0 inside a table it is always smaller than 1.0. So this function will
-- return tabular in case of a nested table, which is necessary since longtables
-- can not be nested inside other longtables, but tabulars can be nested within
-- longtables as well as tabulars.
tableEnvironment ::
  -- | The scaling factor
  Float ->
  -- | The name of the table environment for use in the LaTeX document.
  String
tableEnvironment 1.0 = "longtable"
tableEnvironment _ = "tabular"

-- | Return the inner part of the table header of a table for the LaTeX document
innerTableSpecifier ::
  -- | The number of columns in the table
  Int ->
  -- | The list of column widths of the table in units of the linewidth .
  [Float] ->
  -- | Column separator. Vertical bar if rules shall be drawn, empty string
  -- otherwise.
  String ->
  -- | The inner part of the table header of the table for the LaTeX document
  String
innerTableSpecifier 1 (f : xs) t =
  ">{\\RaggedRight}p{"
    ++ (printf "%0.5f" f)
    ++ "\\linewidth}"
    ++ t
    ++ (innerTableSpecifier 1 xs t)
innerTableSpecifier i (f : xs) t =
  ">{\\RaggedRight}p{\\dhuncolwidth{"
    ++ (printf "%0.5f" f)
    ++ "}}"
    ++ t
    ++ (innerTableSpecifier i xs t)
innerTableSpecifier _ [] _ = []

-- | Returns the table header which represents the width of the columns of a
-- table in units of the line width.  It takes a list of widths as second
-- parameter. It is understood that necessary correction for the width to
-- compensate for the space needed by separations of columns have already been
-- applied. The is the first boolean parameter is true rules will be drawn in
-- the table, otherwise they won't. See also documentation of the
-- `LatexRenderer.wdth3` function in the module `LatexRenderer`.
tableSpecifier ::
  -- | The number of columns in the table.
  Int ->
  -- | True if the rules of the table shall be drawn, False otherwise.
  Bool ->
  -- | The list of column width of the table.
  [Float] ->
  -- | The table header for use in the LaTeX document.
  String
tableSpecifier i True f = '|' : (innerTableSpecifier i f "|")
tableSpecifier i False f = (innerTableSpecifier i f "")

-- | Takes the multirowmap as first input parameter. See documentation on the
-- function `multiRowDictChangeStart` in this module for details on the
-- multirowmap. It returns true if there are currently no multirow cells active
-- in the given multirowdict
myempty ::
  -- | The multirowmap, see `multiRowDictChangeStart`
  Map Int (Int, Int) ->
  -- | True if the a no active cells in the multirowmap given in the first
  -- parameter.
  Bool
myempty d = [x | x <- Map.toList d, (fst (snd x)) /= 0] == []

-- | Takes the string found in the header symbol of a table or the opening tag
-- of the HTML table tag. That is the place where the attributes are, but only
-- understood as string so without parsing the attributes as map, and returns a
-- boolean. It this boolean is true the rules of the table need to be drawn,
-- otherwise they must not be drawn
seperatingLinesRequested ::
  -- | Contents of the opening HTML tag for a table.
  String ->
  -- | True if rules of the table shall be drawn, False otherwise.
  Bool
seperatingLinesRequested s =
  (isInfixOf "Prettytable" (map toLower s))
    || (isInfixOf "prettytable" (map toLower s))
    || (isInfixOf "wikitable" (map toLower s))

-- | Returns the latex symbol for a horizontal line on the last row of a table,
-- that is a horizontal rule, if the first boolean parameter is true, otherwise
-- the empty string is returned. This function is usually being called with the
-- first parameter indicating whether or not rules should be drawn with the
-- table
rowDelimiter ::
  -- | True if the rules in the table shall be drawn, False otherwise.
  Bool ->
  -- | The end symbol for the last line of the table for use in the LaTeX
  -- document.
  String
rowDelimiter True = "\\\\ \\hline"
rowDelimiter False = ""

-- | Returns the latex symbol for a horizontal line in a table, that is a
-- horizontal rule, if the first boolean parameter is true, otherwise the empty
-- string is returned. This function is usually being called with the first
-- parameter indicating whether or not rules should be drawn with the table
horizontalLine ::
  -- | True if rule in the table shall be drawn, False otherwise
  Bool ->
  -- | the LaTeX Command to draw a horizontal rule, if the first parameter is
  -- True, the empty string otherwise.
  String
horizontalLine True = " \\hline"
horizontalLine False = ""

-- | Return the latex symbol for a partly drawn  inner horizontal line in a
-- table, that is a horizontal rule. It has to be drawn only partly since
-- multirow cells intersect with it.
makeCLines ::
  -- | an index that is incremented during the course of this function and has
  -- to be 1 when called this function from outside
  Int ->
  -- | the multirowmap (the documentation on the function
  -- `multiRowDictChangeStart` in this module for details).
  Map Int (Int, Int) ->
  -- | The total number of columns in the table.
  Int ->
  -- | The latex symbol for a partly drawn  inner horizontal line  in a table.
  [Char]
makeCLines m d t =
  if m <= t
    then fromMaybe def $
      do
        (a, b) <- Map.lookup m d
        guard (a /= 0)
        return $ makeCLines (m + (if b > 0 then b else 1)) d t
    else ""
  where
    def =
      "\\cline{"
        ++ (show m)
        ++ "-"
        ++ (show m)
        ++ "}"
        ++ (makeCLines (m + 1) d t)

-- | Return the latex symbol for an inner horizontal line in a table, that is a
-- horizontal rule. If multirow cells interfere with this rule the rule is only
-- drawn in parts as required.
innerHorizontalLine ::
  -- | Boolean parameter: if true the rule is drawn otherwise it is not.
  Bool ->
  -- | The multirowmap (the documentation on the function
  -- `multiRowDictChangeStart` in this module for details).
  Map Int (Int, Int) ->
  -- | The total number of columns in the table.
  Int ->
  -- | the latex symbol for an inner horizontal line in a table.
  String
innerHorizontalLine b d m =
  if b
    then if myempty d then horizontalLine True else ' ' : makeCLines 1 d m
    else ""

-- | The symbol in latex for separating columns. It is returned if the fist
-- boolean parameter is true otherwise the empty string is returned. This
-- function is usually called with the first parameter being true if the current
-- column was not the first column of a row since the symbol is not needed
-- before the start of the first column of a row. This is a contrast to html
-- where the first cell of a row has its own td or th tag. The mediawiki markup
-- notation is similar to html in this respect.
columnSeperator ::
  -- | Boolean parameter:   true if the current column was not the first column
  -- of a row
  Bool ->
  -- | The symbol in latex for separating columns
  String
columnSeperator True = "&"
columnSeperator False = ""

-- | Reads an integer from a map of attributes of th or td tag or corresponding
-- column separator or header column separator at a given key.
genMultiplicity ::
  -- | The key for this map of attributes
  String ->
  -- | The parse result of the attributes of a th tag or td tag or a
  -- corresponding header column separator or column separator containing a map
  -- of attributes
  [Anything Char] ->
  -- | The value for the key (given in the first parameter) in the map of
  -- attributes (given in the second parameter) parsed to an integer an wrapped
  -- in to a Just value of the Maybe monad if the value was found and
  -- successfully parse to an integer. The Nothing value of the Maybe monad
  -- otherwise.
  Maybe Int
genMultiplicity s = msum . (map f)
  where
    f (Environment Attribute (Attr (k, v)) _) =
      listToMaybe $
        do
          (z, _) <- reads v
          guard (k == s)
          return z
    f _ = Nothing

-- | Reads a string from  It takes a key for this map of attributes as string as
-- first parameter wrapped into the Maybe monad.
genLookup ::
  -- | The key for this map of attributes
  String ->
  -- | The parse result of the attributes of a th tag or td tag or a
  -- corresponding header column separator or column separator.
  [Anything Char] ->
  -- | The result of a lookup of the key in the map (so the value) as string
  -- wrapped into a Just value of the Maybe monad. If no value for the key could
  -- be found in the map it returns the value Nothing of the Maybe monad.
  Maybe String
genLookup s = msum . (map f)
  where
    f (Environment Attribute (Attr (k, v)) _) =
      listToMaybe $
        do
          return ()
          guard (k == s)
          return v
    f _ = Nothing

-- | Returns the column multiplicity of the current cell.
columnMultiplicity ::
  -- | The parse result of the inner part of the column separator or header
  -- column separator, that corresponds to the attributes of the th or td HTML
  -- element
  [Anything Char] ->
  -- | The column multiplicity of the current cell wrapped into a Just value of
  -- the Maybe Monad if found. The value Nothing of the Maybe monad is returned
  -- if the attribute colspan is not present (or did not have a value parseable
  -- as Integer) within the first parameter
  Maybe Int
columnMultiplicity = genMultiplicity "colspan"

-- | Returns the row multiplicity of the current cell.

-- | The row multiplicity of the current cell.
rowMultiplicity ::
  -- | The parse result of the inner part of the column separator or header
  -- column separator, that corresponds to the attributes of the th or td HTML
  -- element.
  [Anything Char] ->
  -- | The row multiplicity of the current cell wrapped into a Just value of the
  -- Maybe Monad if found. The value Nothing of the Maybe monad is returned if
  -- the attribute rowspan is not present (or did not have a value parseable as
  -- Integer) within the first parameter
  Maybe Int
rowMultiplicity = genMultiplicity "rowspan"

-- | The column multiplicity of the current cell.
columnMultiplicityForCounting ::
  -- | The parse result of the inner part of the column separator or header
  -- column separator, that corresponds to the attributes of the th or td HTML
  -- elements
  [Anything Char] ->
  -- | The column multiplicity of the current cell if found. The value one is
  -- returned if the attribute colspan is not present (or did not have a value
  -- parseable as Integer) within the first parameter.
  Int
columnMultiplicityForCounting = (fromMaybe 1) . columnMultiplicity

-- | Return the symbol for the start of a multicolumn cell in latex.
multiColumnStartSymbol ::
  -- | the parse result of the inner part of the column separator of header
  -- column separator, that corresponds to the attributes of the th or td HTML
  -- elements.
  [Anything Char] ->
  -- | The nesting depth of table. Is equal to zero the current table is not a
  -- inner table of an outer table.
  Int ->
  -- | The list of the final widths of all columns of the table
  [Float] ->
  -- | The column index of the current column
  Int ->
  -- | A boolean parameter. If it is true rules will be drawn in the table
  -- otherwise they won't.
  Bool ->
  -- | The fifth parameter is the table state, that is the mutable state that
  -- exists during rendering of a table.
  TableState ->
  -- | The symbol for the start of a multicolumn cell in latex.
  String
multiColumnStartSymbol l j f i t st =
  fromMaybe "" $
    do
      n <- columnMultiplicity l
      return $ "\\multicolumn{" ++ (show n) ++ "}{" ++ (spec n) ++ "}{"
  where
    spec mm =
      case activeColumn st of
        Nothing -> tableSpecifier j t (mylist mm)
        _ -> "l"
    mylist nn =
      [ min
          1.0
          ( ((1.0 - (scalefactor 1)) * (sum (take nn (drop (i - 1) f))))
              / (1.0 - (scalefactor (fromIntegral nn)))
          )
      ]

-- | Return the symbol for the end of a multicolumn cell in LaTeX.
multiColumnEndSymbol ::
  -- | Boolean parameter, true if the cell is actually a multicolumn cell.
  Bool ->
  -- | The symbol for the end of a multicolumn cell in latex, if the cell is a
  -- multicolumn cell. The empty string otherwise.
  String
multiColumnEndSymbol True = "}"
multiColumnEndSymbol False = ""

-- | Return the symbol for the end of a multirow cell in LaTeX.
multiRowEndSymbol ::
  -- | Boolean parameter, true if the cell is actually a multirow cell
  Bool ->
  -- | The symbol for the end of a multirow cell in LaTeX, if the cell is a
  -- multirow cell. The empty string otherwise.
  String
multiRowEndSymbol True = "}"
multiRowEndSymbol False = ""

-- | It tries to find a two tuple of Ints (that is value) in the map (given in
-- the firth parameter) under the given key (given in the fourth parameter). If
-- it finds one and the predicate (given in the second parameter) returns true
-- on the first element of that two tuple it returns the result of the function
-- (given in the third parameter) applied on the two tuple (the value). In any
-- other case it returns the default value
withDefault ::
  -- | The default value
  t ->
  -- | A predicate from int to boolean to be applied to the first element of the
  -- two tuple given as value in the map given as fifth parameter
  (Int -> Bool) ->
  -- | a function mapping a two tuple of Int to the same type as the default
  -- value
  (Int -> Int -> t) ->
  -- | a key for that map given in the fifth parameter
  Int ->
  -- | a map from Int to a two tuple of Int
  Map Int (Int, Int) ->
  -- | the result
  t
withDefault def p f i d =
  fromMaybe def $
    do
      (a, b) <- Map.lookup i d
      guard $ p a
      return $ f a b

-- | This function returns the vertical separator symbol for column for the
-- table header in LaTeX.
verticalSeperator ::
  -- | Boolean parameter. True if the rules in the table shall be drawn.
  Bool ->
  -- | vertical separator symbol for column in LaTeX.
  [Char]
verticalSeperator True = "|"
verticalSeperator False = ""

-- | The function returns the a string to be inserted into a latex document for
-- multirows when a new column (that is a new cell) starts, that's when a column
-- separator, or header column separator is encountered.
multiRowSymbol ::
  -- | index of the current column.
  Int ->
  -- | The multirowdict (see documentation of the `multiRowDictChangeStart`
  -- function in this module).
  Map Int (Int, Int) ->
  -- | A boolean. If it is true rules will be drawn in the table, otherwise they
  -- won't-
  Bool ->
  -- | The multi row symbol.
  String
multiRowSymbol i d t = s ++ (if (any (>= i) (keys d)) && (not (((Map.lookup (i + bb) d) >>= (\(g, _) -> Just g)) `elem` [Nothing, Just 0])) then (multiRowSymbol (i + (if bb == 0 then 1 else bb)) d t) else "")
  where
    (bb, s) =
      ( withDefault
          (0, "")
          (> 0)
          ( \_ b ->
              ( b,
                "\\multicolumn{"
                  ++ (show b)
                  ++ "}{"
                  ++ (if (i == 1) then (verticalSeperator t) else "")
                  ++ "c"
                  ++ (verticalSeperator t)
                  ++ "}{}&"
              )
          )
          i
          d
      )

-- | The function returns the a string to be inserted into a latex document for
-- multirows when a new row starts, that's when a row separator is encountered.
multiRowSymbolForRowSep ::
  -- | The index of the current column.
  Int ->
  -- | The multirowdict (see documentation of the `multiRowDictChangeStart`
  -- function in this module).
  Map Int (Int, Int) ->
  -- | A boolean. If it is true rules will be drawn in the table, otherwise they
  -- won't.
  Bool ->
  -- | The multi row symbol for a row separator.
  String
multiRowSymbolForRowSep i d t = (if (intercalate "&" (multiRowSymbolForRowSepInner i d t)) == "" then "" else "&") ++ (intercalate "&" (multiRowSymbolForRowSepInner i d t))

-- | The function returns the the inner parts of the string to be inserted into
-- a latex document for multirows when a new row starts, that's when a row
-- separator is encountered.
multiRowSymbolForRowSepInner ::
  -- | The index of the current column
  Int ->
  -- | The multirowdict (see documentation of the `multiRowDictChangeStart`
  -- function in this module).
  Map Int (Int, Int) ->
  -- | A boolean. If it is true rules will be drawn in the table, otherwise they
  -- won't.
  Bool ->
  -- | The inner parts of the of the string to be inserted into a latex document
  -- for multirows when a new row starts
  [String]
multiRowSymbolForRowSepInner i d t =
  (if s == "" then [] else [s]) ++ (if (any (>= i) (keys d)) && (not (((Map.lookup (i + bb) d) >>= (\(g, _) -> Just g)) `elem` [Nothing, Just 0])) then (multiRowSymbolForRowSepInner (i + (if bb == 0 then 1 else bb)) d t) else [])
  where
    (bb, s) =
      ( withDefault
          (0, "")
          (> 0)
          ( \_ b ->
              ( b,
                "\\multicolumn{"
                  ++ (show b)
                  ++ "}{"
                  ++ (if (i == 1) then (verticalSeperator t) else "")
                  ++ "c"
                  ++ (verticalSeperator t)
                  ++ "}{}"
              )
          )
          i
          d
      )

-- | The function returns the a string to be inserted into a latex document for
-- multirows at the end of the table.
multiRowSymbolForTableEnd ::
  -- | the index of the current column.
  Int ->
  -- | The multirowdict (see documentation of the `multiRowDictChangeStart`
  -- function in this module)
  Map Int (Int, Int) ->
  -- | A boolean. If it is true rules will be drawn in the table, otherwise they
  -- won't.
  Bool ->
  -- | A string to be inserted into a latex document for multirows at the end of
  -- the table.
  String
multiRowSymbolForTableEnd = multiRowSymbolForRowSep

-- | In case of a multirow, that cell has to be skipped further down. So if I
-- got a multirow in row 1 column 2 with a rowspan of 2 or more I need to expand
-- row 2 column 1 by 1 . So if I passed row 2 column one I am not in row 2
-- column 2 since that is where the multirow cell resides, I am rather in row 2
-- cell 3. And if there are more multicolumns involved I am more possible even
-- further right. So this function just tell me how many cells I have to skip.
multiRowCount ::
  -- | the index of the current column.
  Int ->
  -- | is the multirowdict. See also documentation on the function
  -- multiRowDictChangeStart in this module.
  Map Int (Int, Int) ->
  -- | The increase of the row counter for the current cell.
  Int
multiRowCount i d =
  (withDefault 0 (/= 0) (\_ b -> b) i d) + (if (h /= 0) && any (>= i) (keys d) && (not (((Map.lookup (i + h) d) >>= (\(g, _) -> Just g)) `elem` [Nothing, Just 0])) then (multiRowCount (i + h) d) else 0)
  where
    h = (withDefault 0 (/= 0) (\_ b -> b) i d)

-- | Calculates the change of the multiRowDict for cells ending in the current
-- cell. See also documentation on `multiRowDictChangeStart`. This function
-- takes the multiRowDict as second parameter and returns the modified version
-- of it accounting for the cells ending in the current cell.
multiRowDictChangeEnd ::
  -- | The index of the current column.
  Int ->
  -- | The multiRowDict (see documentation on `multiRowDictChangeStart`) in this
  -- module.
  Map Int (Int, Int) ->
  -- | the modified version of the multiRowDict accounting for its cells ending
  -- in the current cell.
  Map Int (Int, Int)
multiRowDictChangeEnd i d =
  if (any (>= i) (keys d)) && (not (((Map.lookup (i + bb) xx) >>= (\(g, _) -> Just g)) `elem` [Nothing, Just 0])) then multiRowDictChangeEnd (i + bb) xx else xx
  where
    xx :: Map Int (Int, Int)
    (bb, xx) = withDefault (0, d) (/= 0) (\a b -> (b, ((Map.insert i (a - 1, b))) d)) i d

-- | The multiRowDict is a facility for keeping track of cells spanning multiple
-- rows. It is stored as mutable state in the type TableState in the parameter
-- multiRowMap. It is passed to this function as second parameter. This function
-- returns an updated version of it. It is a map mapping of an Int to a tuple
-- whose both elements are also Ints. It the key is the column index and the
-- value is a pair (rowmultiplicity, columnmultiplicity). The rowmultiplicity is
-- the number of rows the cell spans. This number is decrease every time a row
-- ends. So it actually says how many rows the columns spans further down from
-- the current column. The column multiplicity is the number of columns the cell
-- spans. This function calculates only the changes in the multirowdict for the
-- opening environment of the cell. You should not use this function but rather
-- use multiRowDictChange since this also considers the effect by ending of
-- cells
multiRowDictChangeStart ::
  -- | The index of the current column.
  Int ->
  -- | The multiRowDict. (see explanation below).
  Map Int (Int, Int) ->
  -- | The parse result of the opening part of the cell environment of the
  -- current cell.
  [Anything Char] ->
  -- | The updated multirow map where the updates due to opening multirows given
  -- in the third parameter have been taken into account, but not the ones due
  -- to closing multirow cells.
  Map Int (Int, Int)
multiRowDictChangeStart i d l =
  fromMaybe d $
    do
      n <- rowMultiplicity l
      return (Map.insert i ((n - 1), c) d)
  where
    c = (columnMultiplicityForCounting l)

-- | Calculate the full change to the multirowdict. See documentation on the
-- function `multiRowDictChangeStart` in this module for more information on the
-- multirowdict. This function returns the updated multirowdict.
multiRowDictChange ::
  -- | The index of the current column.
  Int ->
  -- | The current multirowdict.
  Map Int (Int, Int) ->
  -- | The parse result of the opening part of the cell environment of the
  -- current cell
  [Anything Char] ->
  -- | The updated multirow map where the updates due to opening multirows given
  -- in the third parameter as well as the changes due to multirow cells ending
  -- in the current cell have been taken into account.
  Map Int (Int, Int)
multiRowDictChange i d l =
  multiRowDictChangeStart n (multiRowDictChangeEnd i d) l
  where
    n = i + (multiRowCount i d)

-- | Returns the latex symbol for the start of a multirow cell. That is a cell
-- spanning multiple rows.
multiRowStartSymbol ::
  -- | The inner parse result of the row separator containing information on
  -- whether or not the cell is multirow
  [Anything Char] ->
  -- | activeColumn. This is an integer wrapped in the maybe monad. If it is has
  -- the value Just then the row is to be rendered in a special mode. This mode
  -- is needed to determine the width of the columns. In this special mode no
  -- line break occur and width of the paper is infinite, so that the width of
  -- the of each column is its respective natural width. So there is no limit on
  -- the width of the column. If the value is Nothing this means that the table
  -- is typeset in normal mode and the width is to be limited.
  Maybe Int ->
  -- | The state of the renderer
  MyState ->
  -- | The symbol for a start of a multirow (a cell spanning multiple rows).
  String
multiRowStartSymbol l m st =
  fromMaybe "" $
    do
      n <- rowMultiplicity l
      return $
        if n == 1
          then ""
          else
            ( "\\multirow{"
                ++ (show n)
                ++ "}{"
                ++ (if isJust m then "*" else (printf "%0.5f" (getF st)) ++ "\\linewidth")
                ++ "}"
            )
              ++ "{"

-- | A symbol to be added at the end of header cell in order to make its content
-- bold. The only  Otherwise the empty string is returned.
headendsym ::
  -- | Boolean parameter. It is true if the cell currently under consideration
  -- is a header cell.
  Bool ->
  -- | The symbol for the end of the header cell if the current cell is a header
  -- cell. Otherwise the empty string.
  String
headendsym False = ""
headendsym True = "}"

-- | A symbol to be added at the start of header cell in order to make its
-- content bold
headstartsym ::
  -- | Symbol to make a header cell bold.
  String
headstartsym = "{\\bfseries "

-- | The symbol to be inserted into the LaTeX document, for the end of a row in
-- a table.  Usually if both boolean parameters are true an endhead command
-- should be inserted so that the header of table is repeated on each page. This
-- feature has been disabled, since some authors use new headings in the middle
-- of the table, which causes the repeated heading to be incorrect since they
-- just refer to the first contiguous header rows.
rowendsymb ::
  -- | Boolean parameter. True if the current table is not nested in an other
  -- one.
  Bool ->
  -- | Boolean parameter. True if the column is the last column of the header of
  -- the table.
  Bool ->
  -- | The multiRowMap (see `multiRowDictChangeStart` for details on that). If
  -- it is not empty a multirow cell is currently active and no pagebreaks
  -- should occur. So we tell LaTeX not do any pagebreaks by writing \\* instead
  -- of \\ as row end symbol in this case. See also documentation of the
  -- longtable latex package
  Map Int (Int, Int) ->
  -- | The LaTeX symbol for the end of a row.
  String
rowendsymb _ _ m = "\\\\" ++ (if isEmpty then "" else "*")
  where
    isEmpty = (sum (map fst (Map.elems m))) == 0
