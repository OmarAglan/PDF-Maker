{-DHUN| helper functions to convert tables from the parse tree notation to the latex notation DHUN-}
module TableHelper where
import MediaWikiParseTree
import Tools
import Text.Printf
import Data.Char
import qualified Data.Map as Map
import Data.Map.Strict (Map,keys)
import Data.Maybe
import Control.Monad
import MyState
import Data.List (intercalate)
{-DHUN| the width of a columns as float wrapped in a Just value of the maybe monad if it could be determined. Return the value Nothing of the maybe monad otherwise. The only parameter is of part of the parse tree that describe the opening part of the table cell element. DHUN-}

widthOfColumn :: [Anything Char] -> Maybe Float
widthOfColumn = msum . (map f)
  where f (Environment Attribute (Attr (k, v)) _)
          = listToMaybe $
              do (z, _) <- reads v
                 guard (k == "width")
                 guard ('%' `elem` v)
                 return (1.0e-2 * z)
        f _ = Nothing

columnMultiplicityForSimple :: [Anything Char] -> Int
columnMultiplicityForSimple x
  = case columnMultiplicity x of
        Just a -> a
        _ -> -1

raggedArrayOfWidths ::
                    [Anything Char] -> [Maybe Float] -> [[Maybe Float]]
raggedArrayOfWidths ((Environment TableRowSep _ _) : xs) temp
  = if temp == [] then raggedArrayOfWidths xs [] else
      temp : (raggedArrayOfWidths xs [])
raggedArrayOfWidths ((Environment TableColSep _ x) : xs) temp
  = (raggedArrayOfWidths xs
       (temp ++
          [widthOfColumn x] ++
            (concat
               (replicate ((columnMultiplicityForSimple x) - 1) [Nothing]))))
raggedArrayOfWidths ((Environment TableHeadColSep _ x) : xs) temp
  = (raggedArrayOfWidths xs
       (temp ++
          [widthOfColumn x] ++
            (concat
               (replicate ((columnMultiplicityForSimple x) - 1) [Nothing]))))
raggedArrayOfWidths (_ : xs) temp = (raggedArrayOfWidths xs temp)
raggedArrayOfWidths [] temp = [temp]

numberOfColumns :: [Anything Char] -> Int
numberOfColumns a
  = (maximum ([length x | x <- (raggedArrayOfWidths a [])]))

initialListofWidths :: [Anything Char] -> [Maybe Float]
initialListofWidths x = replicate (numberOfColumns x) Nothing

listMax :: [Maybe Float] -> [Maybe Float] -> [Maybe Float]
listMax (Just x : xs) (Just y : ys)
  = Just (max x y) : listMax xs ys
listMax (x : xs) (y : ys) = (x `mplus` y) : listMax xs ys
listMax [] (y : ys) = y : listMax [] ys
listMax (x : xs) [] = x : listMax xs []
listMax [] [] = []

preliminaryWidths ::
                  [[Maybe Float]] -> [Maybe Float] -> [Maybe Float]
preliminaryWidths l k = foldl (listMax) k l

standardColumnWitdh :: [Anything Char] -> Maybe Float
standardColumnWitdh a
  = if columns > columnsWithDefinedWidth then
      if sumOfDefinedWidths > 1.0 then Nothing else
        Just $
          (1.0 - sumOfDefinedWidths) /
            fromIntegral (columns - columnsWithDefinedWidth)
      else if sumOfDefinedWidths > 1.001 then Nothing else Just 0.0
  where l = rawWidths a
        columns = numberOfColumns a
        columnsWithDefinedWidth = length (filter isJust l)
        sumOfDefinedWidths = sum (map fromJust (filter isJust l))

rawWidths :: [Anything Char] -> [Maybe Float]
rawWidths a
  = (preliminaryWidths (raggedArrayOfWidths a [])
       (initialListofWidths a))

{-DHUN fallback function for the width of columns if the precompilation procedure for the width of columns is not available yet. So particularly when the precompilation is just running. Takes the parse tree representation of the table as first input parameter. Returns the list of widths of the columns. I am not documenting its subfunctions since the final width of the columns don't have anything to do with the width calculated here, still these preliminary widths are needed for the precompilation procedure DHUN-}

columnWidths :: [Anything Char] -> [Float]
columnWidths a = w
  where l = rawWidths a
        m = numberOfColumns a
        mf = fromIntegral m
        f = 1.0 - (scalefactor mf)
        w = fromMaybe (concat (replicate m [f / mf])) $
              do ww <- standardColumnWitdh a
                 return [x * f | x <- map (fromMaybe ww) l]

{-DHUN| part of the correction calculation for the space between columns inside a table. Takes the number of columns as first input parameter returns a scaling factor DHUN-}

scalefactor :: (Fractional a, Ord a) => a -> a
scalefactor n | n <= 10 = 12.8 * (n) / 448.0
scalefactor _ = 12.8 * (11.0) / 448.0

{-DHUN| part of the correction calculation for the space between columns inside a table. Takes the number of columns as first input parameter returns a scaling factor DHUN-}

tableScale :: Int -> Float
tableScale nColumns = (1.0 / n) * (1.0 - (scalefactor n))
  where n = fromIntegral nColumns

{-DHUN| returns the latex environment name for a table environments. It the float passes as first parameter it 1.0 the result is longtable, otherwise it is tabular. This function is usually called with the width of the current cell in units of the line width as first parameter. Outside any table this value is 1.0 inside a table it is always smaller than 1.0. So this function will return tabular in case of a nested table, which is necessary since longtables can not be nested inside other longtables, but tabulars can be nested within longtables as well as tabulars. DHUN-}

tableEnvironment :: Float -> String
tableEnvironment 1.0 = "longtable"
tableEnvironment _ = "tabular"

innerTableSpecifier :: [Float] -> String -> String
innerTableSpecifier (f : xs) t
  = ">{\\RaggedRight}p{" ++
      (printf "%0.5f" f) ++
        "\\linewidth}" ++ t ++ (innerTableSpecifier xs t)
innerTableSpecifier [] _ = []

{-DHUN| Returns the table header which represents the width of the columns of a table in units of the line width.  It takes a list of width as second parameter. It is understood that necessary correction for the width to compensate for the space needed by separations of columns have already been applied. The is the first boolean parameter is true rules will be drawn in the table, otherwise they won't. See also documentation of the wdth3 function in the module LatexRenderer.  DHUN-}

tableSpecifier :: Bool -> [Float] -> String
tableSpecifier True f = '|' : (innerTableSpecifier f "|")
tableSpecifier False f = (innerTableSpecifier f "")

{-DHUN| Takes the multirowmap as first input parameter. See documentation on the function multiRowDictChangeStart in this module for details on the multirowmap. It returns true if there are currently no multirow cells active in the given multirowdict DHUN-}

myempty :: Map Int (Int, Int) -> Bool
myempty d = [x | x <- Map.toList d, (fst (snd x)) /= 0] == []

{-DHUN| takes the string found in the header symbol of a table or the opening tag of the html table tag. That is the place where the attributes are, but only understood as string so without parsing the attributes as map, and returns a boolean. It this boolean is true the rules of the table need to be drawn, otherwise they must not be drawn DHUN-}

seperatingLinesRequested :: String -> Bool
seperatingLinesRequested s
  = (isInfixOf2 "Prettytable" (map toLower s)) ||
      (isInfixOf2 "prettytable" (map toLower s)) ||
        (isInfixOf2 "wikitable" (map toLower s))

{-DHUN| returns the latex symbol for a horizontal line on the last row of a table, that is a horizontal rule, if the first boolean parameter is true, otherwise the empty string is returned. This function is usually being called with the first parameter indicating whether or not rules should be drawn with the table DHUN-}

rowDelimiter :: Bool -> String
rowDelimiter True = "\\\\ \\hline"
rowDelimiter False = ""

{-DHUN| returns the latex symbol for a horizontal line in a table, that is a horizontal rule, if the first boolean parameter is true, otherwise the empty string is returned. This function is usually being called with the first parameter indicating whether or not rules should be drawn with the table DHUN-}

horizontalLine :: Bool -> String
horizontalLine True = " \\hline"
horizontalLine False = ""

{-DHUN| return the latex symbol for a partly drawn  inner horizontal line in a table, that is a horizontal rule. It has to be drawn only partly since multirow cells intersect with it.  The second parameter is the multirowmap (the documentation on the function multiRowDictChangeStart in this module for details). The third parameter is the total number of columns in the table. The first parameter is a and index that is incremented during the course of this function and has to be 1 when called this function from outside DHUN-}

makeCLines :: Int -> Map Int (Int, Int) -> Int -> [Char]
makeCLines m d t
  = if m <= t then
      fromMaybe def $
        do (a, b) <- Map.lookup m d
           guard (a /= 0)
           return $ makeCLines (m + (if b > 0 then b else 1)) d t
      else ""
  where def
          = "\\cline{" ++
              (show m) ++ "-" ++ (show m) ++ "}" ++ (makeCLines (m + 1) d t)

{-DHUN| return the latex symbol for an inner horizontal line in a table, that is a horizontal rule. If the first boolean parameter is true the rule is drawn otherwise it is not. If multirow cells interfere with this rule the rule is only drawn in parts as required. The second parameter is the multirowmap (the documentation on the function multiRowDictChangeStart in this module for details). The third parameter is the total number of columns in the table DHUN-}

innerHorizontalLine :: Bool -> Map Int (Int, Int) -> Int -> String
innerHorizontalLine b d m
  = if b then
      if myempty d then horizontalLine True else ' ' : makeCLines 1 d m
      else ""

{-DHUN| the symbol in latex for separating columns. It is returned if the fist boolean parameter is true otherwise the empty string is returned. This function is usually called with the first parameter being true if the current column was not the first column of a row since the symbol is not needed before the start of the first column of a row. This is a contrast to html where the first cell of a row has its own td or th tag. The mediawiki markup notation is similar to html in this respect. DHUN-}

columnSeperator :: Bool -> String
columnSeperator True = "&"
columnSeperator False = ""

{-DHUN| takes the parse result of the attributes of a th tag or td tag or a corresponding header column separator or column separator as second parameter. It takes a key for this map of attributes as string as first parameter. If the key is present in the map, and the value found in the map at that key can be parsed as an Integer that integer is returned. If no value for the key could be found in the map or it could not be parsed as an integer the value Nothing of the Maybe monad is returned. DHUN-}

genMultiplicity :: String -> [Anything Char] -> Maybe Int
genMultiplicity s = msum . (map f)
  where f (Environment Attribute (Attr (k, v)) _)
          = listToMaybe $
              do (z, _) <- reads v
                 guard (k == s)
                 return z
        f _ = Nothing

{-DHUN| takes the parse result of the attributes of a th tag or td tag or a corresponding header column separator or column separator as second parameter. It takes a key for this map of attributes as string as first parameter. It returns a result of the lookup of the key in the map (so the value). as string wrapped into the Maybe monad. If no value for the key could be found in the map it returns the value Nothing of the Maybe monad. DHUN-}

genLookup :: String -> [Anything Char] -> Maybe String
genLookup s = msum . (map f)
  where f (Environment Attribute (Attr (k, v)) _)
          = listToMaybe $
              do return ()
                 guard (k == s)
                 return v
        f _ = Nothing

{-DHUN| the column multiplicity of the current cell. The first parameter is the parse result of the inner part of the column separator of header column separator, that corresponds to the attributes of the th or td html elements. The result is an integer wrapped into the maybe monad. The value Nothing of the Maybe monad is returned if the attribute colspan is not present (or did not have a value parseable as Integer) within the first parameter DHUN-}

columnMultiplicity :: [Anything Char] -> Maybe Int
columnMultiplicity = genMultiplicity "colspan"

{-DHUN| the row multiplicity of the current cell. The first parameter is the parse result of the inner part of the column separator of header column separator, that corresponds to the attributes of the th or td html elements. The result is an integer wrapped into the maybe monad. The value Nothing of the Maybe monad is returned if the attribute rowspan is not present (or did not have a value parseable as Integer) within the first parameter DHUN-}

rowMultiplicity :: [Anything Char] -> Maybe Int
rowMultiplicity = genMultiplicity "rowspan"

{-DHUN| the column multiplicity of the current cell. The first parameter is the parse result of the inner part of the column separator of header column separator, that corresponds to the attributes of the th or td html elements.The result is an integer that default to zero DHUN-}

columnMultiplicityForCounting :: [Anything Char] -> Int
columnMultiplicityForCounting = (fromMaybe 1) . columnMultiplicity

{-DHUN| return the symbol for the start of a multicolumn cell in latex. The first parameter is the parse result of the inner part of the column separator of header column separator, that corresponds to the attributes of the th or td html elements. It takes the list of the final widths of all columns of the table as second parameter. It takes to the column index of the current column as third parameter. The fourth parameter is a boolean if it is true rules will be drawn in the table otherwise they won't. The fifth parameter is the table state. That is the mutable state that exists during rendering of a table. DHUN-}

multiColumnStartSymbol ::
                       [Anything Char] -> [Float] -> Int -> Bool -> TableState -> String
multiColumnStartSymbol l f i t st
  = fromMaybe "" $
      do n <- columnMultiplicity l
         return $ "\\multicolumn{" ++ (show n) ++ "}{" ++ (spec n) ++ "}{"
  where spec mm
          = case activeColumn st of
                Nothing -> tableSpecifier t (mylist mm)
                _ -> "l"
        mylist nn
          = [min 1.0
               (((1.0 - (scalefactor 1)) * (sum (take nn (drop (i - 1) f)))) /
                  (1.0 - (scalefactor (fromIntegral nn))))]

{-DHUN| return the symbol for the end of a multicolumn cell in latex. The first boolean parameter tells if the cell is actually a multicolumn cell. If it is false the empty string is returned instead DHUN-}

multiColumnEndSymbol :: Bool -> String
multiColumnEndSymbol True = "}"
multiColumnEndSymbol False = ""

{-DHUN| return the symbol for the end of a multirow cell in latex. The first boolean parameter tells if the cell is actually a multirow cell. If it is false the empty string is returned instead DHUN-}

multiRowEndSymbol :: Bool -> String
multiRowEndSymbol True = "}"
multiRowEndSymbol False = ""

{-DHUN| This function takes a default value as first parameter. It takes a predicate as second parameter. It take a map from Int to a two tuple of Int as firth parameter. It takes a key for that map as fourth parameter. It takes a function mapping a two tuple of Int to the same type as the default value as third parameter. It tries to find a two tuple of Ints (that is value) in the map under the given key. If it finds one and the predicate returns true on the first element of that two tuple it returns the result of the function on the two tuple. In any other case it returns the default value DHUN-}

withDefault ::
            t ->
              (Int -> Bool) ->
                (Int -> Int -> t) -> Int -> Map Int (Int, Int) -> t
withDefault def p f i d
  = fromMaybe def $
      do (a, b) <- Map.lookup i d
         guard $ p a
         return $ f a b

{-DHUN| this function return the vertical separator for column for the table header in latex. The only parameter is a boolean. If it is true rules will be drawn in the table, otherwise they won't DHUN-}

verticalSeperator :: Bool -> [Char]
verticalSeperator True = "|"
verticalSeperator False = ""

{-DHUN| the function returns the a string to be inserted into a latex document for multirows at a when a new column (that is a new cell) starts, thats when a column separator, or header column separator is encountered. The first parameter it the index of the current column. The second parameter is the multirowdict (see documentation of the multiRowDictChangeStart function in this module). The third parameter is a boolean. If it is true rules will be drawn in the table, otherwise they won't DHUN-}

multiRowSymbol :: Int -> Map Int (Int, Int) -> Bool -> String
multiRowSymbol i d t= s  ++ (if (any (>=i) (keys d))&&(not(((Map.lookup (i+bb) d)>>= (\(g,_) ->Just g)) `elem` [Nothing,Just 0])) then (multiRowSymbol (i + (if bb==0 then 1 else bb)) d t) else "")

  where
    (bb,s)  = (withDefault (0,"") (> 0)  (\ _ b ->
         (b,"\\multicolumn{" ++
           (show b) ++
             "}{" ++
               (if (i==1) then (verticalSeperator t) else "")++
                 "c" ++
                   (verticalSeperator t) ++ "}{}&" )) i d)

{-DHUN| the function returns the a string to be inserted into a latex document for multirows at a when a new row starts, thats when a row separator is encountered. The first parameter it the index of the current column. The second parameter is the multirowdict (see documentation of the multiRowDictChangeStart function in this module). The third parameter is a boolean. If it is true rules will be drawn in the table, otherwise they won't DHUN-}

multiRowSymbolForRowSep ::
                        Int -> Map Int (Int, Int) -> Bool -> String

multiRowSymbolForRowSep i d t = (if (intercalate "&" (multiRowSymbolForRowSepInner i d t)) =="" then "" else "&")++ (intercalate "&" (multiRowSymbolForRowSepInner i d t))

multiRowSymbolForRowSepInner ::
                        Int -> Map Int (Int, Int) -> Bool -> [String]
multiRowSymbolForRowSepInner i d t
  = (if s=="" then [] else [s]) ++ (if (any (>=i) (keys d))&&(not(((Map.lookup (i+bb) d)>>= (\(g,_) ->Just g)) `elem` [Nothing,Just 0])) then (multiRowSymbolForRowSepInner (i + (if bb==0 then 1 else bb)) d t) else [])
  where 
   (bb,s)=(withDefault (0,"") (> 0)  (\ _ b ->  (b,"\\multicolumn{" ++
           (show b) ++
             "}{" ++
                (if (i==1) then (verticalSeperator t) else "") ++
                 "c" ++
                   (verticalSeperator t) ++
                     "}{}" )) i d)
{-DHUN| the function returns the a string to be inserted into a latex document for multirows at the end of the table. The first parameter it the index of the current column. The second parameter is the multirowdict (see documentation of the multiRowDictChangeStart function in this module). The third parameter is a boolean. If it is true rules will be drawn in the table, otherwise they won't DHUN-}

multiRowSymbolForTableEnd ::
                          Int -> Map Int (Int, Int) -> Bool -> String
multiRowSymbolForTableEnd = multiRowSymbolForRowSep

{-DHUN| in case of a multirow, that cell has to be skipped further down. So if I got a multirow in row 1 column 2 with a rowspan of 2 or more I need to expand row 2 column 1 by 1 . So if I passed row 2 column one I am not in row 2 column 2 since that is where the multirow cell resides, I am rather in row 2 cell 3. And if there are more multicolumns involved I am more possible even further right. So this function just tell me how many cells I have to skip. The first parameter is the index of the current column. The second parameter is the multirowdict. See also documentation on the function multiRowDictChangeStart in this module  DHUN-}

multiRowCount :: Int -> Map Int (Int, Int) -> Int
multiRowCount i d
  = (withDefault 0 (/= 0) (\ _ b -> b) i d) + (if (h/=0)&&any (>=i) (keys d)&&(not(((Map.lookup (i+h) d)>>= (\(g,_) ->Just g)) `elem` [Nothing,Just 0])) then (multiRowCount (i + h) d) else 0)
  where h= (withDefault 0 (/= 0) (\ _ b -> b) i d)

{-DHUN| see documentation on multiRowDictChangeStart. This function take the index of the current column as first parameter. This function takes the multiRowDict as first parameter and returns the modified version of it. DHUN-}

multiRowDictChangeEnd ::
                      Int -> Map Int (Int, Int) -> Map Int (Int, Int)
multiRowDictChangeEnd i d
  =  if  (any (>=i) (keys d))&&(not(((Map.lookup (i+bb) xx)>>= (\(g,_) ->Just g)) `elem` [Nothing,Just 0]))  then multiRowDictChangeEnd (i + bb) xx else xx
    where
      xx::Map Int (Int, Int)
      (bb,xx)= withDefault (0,d) (/= 0)  (\ a b -> (b,( (Map.insert i (a - 1, b))) d)) i d



{-DHUN| The multiRowDict is a facility for keeping track of cells spanning multiple rows. It is stored as mutable state in the type TableState in the parameter multiRowMap. It is passed to this function as second parameter. This function return an updated version of it. It is a map mapping and Int to a tuple whose both elements are also ints. It the key is the column index and the value is a pair (rowmultiplicity, columnmultiplicity). The rowmultiplicity is the number of rows the cell spans. This number is decrease every time a row ends. So it actually says how many rows the columns spans further down from the current column. The column multiplicity is the number of columns the cell spans. This function take the index of the current column as first parameter. This function takes the parse result of the opening part of the cell environment of the current cell as third input parameter. This function calculates only the changes in the multirowdict for the opening environment of the cell. You should not use this function but rather use multiRowDictChange since this also considers the effect by ending of cells DHUN-}

multiRowDictChangeStart ::
                        Int -> Map Int (Int, Int) -> [Anything Char] -> Map Int (Int, Int)
multiRowDictChangeStart i d l
  = fromMaybe d $
      do n <- rowMultiplicity l
         return (Map.insert i ((n - 1), c) d)
  where c = (columnMultiplicityForCounting l)

{-DHUN| calculate the full change to the multirowdict. See documentation on the function multiRowDictChangeStart in this module for more information on the multirowdict. The first parameter is the index of the current column. The second parameter is the current multirowdict. This function takes the parse result of the opening part of the cell environment of the current cell as third input parameter. This function returns the updated multirowdict. DHUN-}

multiRowDictChange ::
                   Int -> Map Int (Int, Int) -> [Anything Char] -> Map Int (Int, Int)
multiRowDictChange i d l
  = multiRowDictChangeStart n (multiRowDictChangeEnd i d) l
  where n = i + (multiRowCount i d)

{-DHUN| returns the latex symbol for the start of a multirow cell. That is a cell spanning multiple rows. The second parameter is activeColumn. This is an integer wrapped in the maybe monad. If it is has the value Just then the row is to be renderer in a special mode. This mode is needed to determine the width of the columns. In this special mode no line break occur and width of the paper is infinite, so that the width of the of each column is its respective natural width. So there is no limit on the width of the column. If the value is Nothing this means that the table is typeset in normal mode and the width is to be limited. The first parameter is the inner parse result of the row separator containing information on whether or not the cell is multirow DHUN-}

multiRowStartSymbol :: [Anything Char] -> Maybe Int -> String
multiRowStartSymbol l m
  = fromMaybe "" $
      do n <- rowMultiplicity l
         return $
           "\\multirow{" ++
             (show n) ++
               "}{" ++ (if isJust m then "*" else "\\linewidth") ++ "}{"

{-DHUN| a symbol to be added at the end of header cell in order to make its content bold. The only boolean parameter is use to indicate whether the cell currently under consideration is a header cell. Otherwise the empty string is returned.  DHUN-}

headendsym :: Bool -> String
headendsym False = ""
headendsym True = "}"

{-DHUN| a symbol to be added at the start of header cell in order to make its content bold DHUN-}

headstartsym :: String
headstartsym = "{\\bfseries "

{-DHUN| the symbol to be inserted into the latex document, for the end of a row in a table. The first boolean parameter is should be true if the current table is not nested in an other one. The second boolean parameter should be true if the column was the last column of the header of the table. The header of the table is repeated by latex each time page wrapping inside the table occurs. See also documentation of the longtable latex package DHUN-}

rowendsymb :: Bool -> Bool -> String
rowendsymb True True = "\\endhead "
rowendsymb _ _ = "\\\\"
