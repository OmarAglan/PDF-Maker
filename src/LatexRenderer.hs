
{-DHUN| This modules converts the parse tree to a latex document DHUN-}
module LatexRenderer
       (treeToLaTeX2, treeToLaTeX3, shallowFlatten, prepateTemplate, replace, doUnicode,
        getGalleryNumbers, getTitle, initialState, getJ, urld,
        analyseNetloc, templateMap, getUserTemplateMap, urls, mUrlState,
        initialUrlState, makeLables, templateRegistry, baseUrl,
        deepFlatten, wikiLinkCaption, imageSize, isCaption, linewidth,
        generateGalleryImageNumbers, splitToTuples, galleryTableScale,
        tempProcAdapter)
       where
import Data.String.HT (trim)
import MyState
import Data.List
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Char
import Text.Printf
import FontTool
import MediaWikiParseTree
import MagicStrings
import Tools
import Control.Monad.Trans.State
       (State, state, runState, StateT, runStateT, put, get)
import Control.Monad.Trans.Class (lift)
import Control.Monad (guard, mplus, msum)
import TableHelper
import GHC.Float
import WikiLinkHelper
import WikiHelper
import Data.List.Split
import BaseFont
import Data.Maybe
import Data.Tuple (swap)
import MediaWikiParser hiding (prep)

{-DHUN|  the maximum width of lines for preformat and source code DHUN-}

linewidth :: Int
linewidth = 80

{-DHUN| The user can provide her own translation table for mediawiki templates to latex commands. this is done in the templates.user files. This function takes this file in list representation and converts it to the map representation to be able to look up the names of templates DHUN-}

getUserTemplateMap :: [[String]] -> Map String [String]
getUserTemplateMap input
  = Map.fromList (map (\ (x : xs) -> (x, xs)) input)

{-DHUN| table may omit tailing columns in a row, but in latex they need to be written out, this function does so DHUN-}

rowaddsym :: TableState -> [Char]
rowaddsym st
  = if (currentColumn st) < ((numberOfColumnsInTable st) + 1) then
      (if (currentColumn st) == 1 then
         replicate (((numberOfColumnsInTable st)) - (currentColumn st)) '&'
         else
         replicate (((numberOfColumnsInTable st)) - (currentColumn st))
           '&')
      else []

{-DHUN| This function renders the inner parts of a table to latex it does so by calling tableContentToLaTeX and additionally removes superfluous newlines which might cause compilation problems in latex when used inside tables DHUN-}

tableContentToLaTeX2 ::
                     [Anything Char] -> (StateT TableState (State MyState) String)
tableContentToLaTeX2 l
  = do r <- tableContentToLaTeX l
       return (killnl2 r)

varwidthbegin :: TableState -> [Char]
varwidthbegin st
  = if isJust (activeColumn st) then "\\begin{varwidth}{\\linewidth}"
      else ""

varwidthend :: TableState -> [Char]
varwidthend st
  = if isJust (activeColumn st) then "\\end{varwidth}" else ""

{-DHUN| This function renders the inner parts of a table to latex, please always use tableContentToLaTeX2 since this also removes superfluous newlines DHUN-}

tableContentToLaTeX ::
                    [Anything Char] -> (StateT TableState (State MyState) String)
tableContentToLaTeX ((Environment TableRowSep _ _) : [])
  = do st <- get
       let cc = (currentColumn st)
       let c = cc + (multiRowCount cc (multiRowMap st))
       return $
         (varwidthend st) ++
           (headendsym (lastCellWasHeaderCell st)) ++
             (multiColumnEndSymbol (lastCellWasMultiColumn st)) ++ (multiRowEndSymbol (lastCellWasMultiRow st)) ++
               (rowaddsym st{currentColumn = c})
tableContentToLaTeX ((Environment TableRowSep _ l) : xs)
  = do sst <- lift get
       st <- get
       let cc = (currentColumn st)
       let c = cc + (multiRowCount cc (multiRowMap st))
       let mycond
             = (not (currentRowIsHeaderRow st)) &&
                 (stillInTableHeader st) &&
                   (lastRowHadEmptyMultiRowMap st) && (not (isFirstRow st))
       put
         (st{rowCounter = 1 + (rowCounter st),
             outputTableHasHeaderRows =
               (outputTableHasHeaderRows st) || (currentRowIsHeaderRow st),
             lastRowHadEmptyMultiRowMap = (myempty (multiRowMap st))})
       st2 <- get
       put
         st2{outputLastRowOfHeader =
               (if mycond then rowCounter st2 else outputLastRowOfHeader st2)}
       st3 <- get
       put
         st3{lastCellWasNotFirstCellOfRow = False,
             lastCellWasMultiColumn = False, currentColumn = 1,
             multiRowMap =
               multiRowDictChange (currentColumn st) (multiRowMap st) l,
             lastCellWasMultiRow = False, isFirstRow = False,
             lastCellWasHeaderCell = False, currentRowIsHeaderRow = False,
             stillInTableHeader =
               if stillInTableHeader st then not mycond else False}
       xx <- tableContentToLaTeX xs
       return $
         if (not (isFirstRow st)) then
           (varwidthend st) ++
             (headendsym (lastCellWasHeaderCell st)) ++
               (multiColumnEndSymbol (lastCellWasMultiColumn st)) ++
                 (multiRowEndSymbol (lastCellWasMultiRow st)) ++
                   (multiRowSymbolForRowSep (currentColumn st) (multiRowMap st)
                      (seperatingLinesRequestedForTable st))
                     ++
                     (rowaddsym st{currentColumn = c}) ++
                       (rowendsymb ((getInTab sst) <= 1)
                          ((rowCounter st) == (inputLastRowOfHeader st) - 2))
                          ++
                         (innerHorizontalLine (seperatingLinesRequestedForTable st)
                            (multiRowMap st3)
                            (numberOfColumnsInTable st))
                           ++ " \n" ++ (varwidthbegin st) ++ xx
           else xx
tableContentToLaTeX ((Environment TableColSep _ l) : xs)
  = do st <- get
       let cc = (currentColumn st)
       let c = cc + (multiRowCount cc (multiRowMap st))
       put
         st{lastCellWasNotFirstCellOfRow = True,
            lastCellWasMultiColumn =
              ("" /=
                 (multiColumnStartSymbol l (columnsWidthList st) c
                    (seperatingLinesRequestedForTable st)
                    st)),
            currentColumn = (c + (columnMultiplicityForCounting l)),
            multiRowMap =
              multiRowDictChange (currentColumn st) (multiRowMap st) l,
            lastCellWasMultiRow =
              (multiRowStartSymbol l (activeColumn st)) /= "",
            isFirstRow = False, lastCellWasHeaderCell = False}
       xx <- tableContentToLaTeX xxs
       return $
         (varwidthend st) ++
           (headendsym (lastCellWasHeaderCell st)) ++
             (multiColumnEndSymbol (lastCellWasMultiColumn st)) ++
               (multiRowEndSymbol (lastCellWasMultiRow st)) ++
                 (columnSeperator (lastCellWasNotFirstCellOfRow st)) ++
                   (multiRowSymbol (currentColumn st) (multiRowMap st)
                      (seperatingLinesRequestedForTable st))
                     ++
                     (multiColumnStartSymbol l (columnsWidthList st) c
                        (seperatingLinesRequestedForTable st)
                        st)
                       ++
                       (multiRowStartSymbol l (activeColumn st)) ++
                         (if rig then "\\RaggedLeft{}" else "") ++
                           (tablecolorsym l) ++ hypennothing ++ (varwidthbegin st) ++ xx
  where rig
          = isInfixOf2
              [Environment Attribute (Attr ("style", "text-align:right")) []]
              l
        xxs
          = if rig then (reverse . removesp . reverse . removesp) xs else xs
        removesp (C ' ' : as) = removesp as
        removesp a = a
tableContentToLaTeX ((Environment TableHeadColSep _ l) : xs)
  = do st <- get
       let cc = currentColumn st
       let c = cc + (multiRowCount cc (multiRowMap st))
       put
         st{lastCellWasNotFirstCellOfRow = True,
            lastCellWasMultiColumn =
              ("" /=
                 (multiColumnStartSymbol l (columnsWidthList st) c
                    (seperatingLinesRequestedForTable st)
                    st)),
            currentColumn = (c + (columnMultiplicityForCounting l)),
            multiRowMap =
              multiRowDictChange (currentColumn st) (multiRowMap st) l,
            lastCellWasMultiRow =
              multiRowStartSymbol l (activeColumn st) /= "",
            isFirstRow = False, lastCellWasHeaderCell = True,
            currentRowIsHeaderRow = True}
       xx <- tableContentToLaTeX xs
       return $
         (varwidthend st) ++
           (headendsym (lastCellWasHeaderCell st)) ++
             (multiColumnEndSymbol (lastCellWasMultiColumn st)) ++
               (multiRowEndSymbol (lastCellWasMultiRow st)) ++
                 (columnSeperator (lastCellWasNotFirstCellOfRow st)) ++
                   (multiRowSymbol (currentColumn st) (multiRowMap st)
                      (seperatingLinesRequestedForTable st))
                     ++
                     (multiColumnStartSymbol l (columnsWidthList st) c
                        (seperatingLinesRequestedForTable st)
                        st)
                       ++
                       (multiRowStartSymbol l (activeColumn st)) ++
                         headstartsym ++
                           (tablecolorsym l) ++ hypennothing ++ (varwidthbegin st) ++ xx
tableContentToLaTeX (x : xs)
  = do st <- get
       ele <- case (activeColumn st) of
                  Just n | (n /= fromIntegral (currentColumn st)) ||
                             (lastCellWasMultiColumn st)
                           -> return []
                  _ -> lift $ treeToLaTeX2 [x]
       xx <- tableContentToLaTeX xs
       return $ ele ++ xx
tableContentToLaTeX []
  = do st <- get
       let cc = currentColumn st
       let c = cc + (multiRowCount cc (multiRowMap st))
       return $
         (varwidthend st) ++
           (headendsym (lastCellWasHeaderCell st)) ++
             (multiColumnEndSymbol (lastCellWasMultiColumn st)) ++                  (multiRowEndSymbol (lastCellWasMultiRow st)) ++
                   (rowaddsym
                      st{currentColumn = (c + (columnMultiplicityForCounting []))}) ++
               (multiRowSymbolForTableEnd (currentColumn st) (multiRowMap st)
                  (seperatingLinesRequestedForTable st))
                 
subTableCellCorrect:: [Anything Char]->[Anything Char]
subTableCellCorrect [] = []
subTableCellCorrect (x:xs) = if look then (smaller upto)++ (subTableCellCorrect (dropWhile p (x:xs))) else x:(subTableCellCorrect xs)
  where 
    p (Environment TableRowSep _ _) = False
    p (Environment TableColSep _ _) = False
    p (Environment TableHeadColSep _ _) = False
    p _ = True
    q (Environment Wikitable _ _) = (1::Integer)
    q _ = 0
    smaller ((Environment Wikitable (TagAttr t a) l):ys) = (Environment Wikitable (TagAttr t (Map.insert "class" "navbox" a)) l):(smaller ys)
    smaller (y:ys) = y:(smaller ys)
    smaller [] = []
    upto = takeWhile p (x:xs)
    look = (sum (map q upto)) >= 2

{-DHUN| This string has to be added to each new cell in a latex table in order to allow for hyphenation of the first word in this cell DHUN-}

hypennothing :: [Char]
hypennothing = "\\hspace*{0pt}\\ignorespaces{}\\hspace*{0pt}"

{-DHUN| color cell in latex if HTML attribute bgcolor is present in the parse tree for the cell DHUN-}

tablecolorsym :: [Anything Char] -> [Char]
tablecolorsym ll
  = case genLookup "bgcolor" ll of
        Just x -> case x of
                      ('#' : ys) -> let (p, colname, col) = colinfo ('l' : 'l' : ys) in
                                      if p then "\\cellcolor[rgb]" ++ col else
                                        "\\cellcolor{" ++ colname ++ "}"
                      _ -> "\\cellcolor{" ++ x ++ "}"
        Nothing -> ""

{-DHUN| the caption of a table is given in |+ or <th> elements, it needs to be reformatted in the parse in oder to be rendered in latex as a multicolumn cell spanning the whole width of the table DHUN-}

reformatTableCaption ::
                     Int -> [Anything Char] -> MyState -> [Anything Char]
reformatTableCaption n
  ((Environment TableCap _ l) : ((Environment TableRowSep a b) : xs))
  st
  = if (filter (not . isSpace) (treeToLaTeX l st)) == [] then
      reformatTableCaption n xs st else
      (Environment TableRowSep (Str "") []) :
        ((Environment TableHeadColSep (Str "")
            [Environment Attribute (Attr ("colspan", (show n))) []])
           : l)
          ++ [(Environment TableRowSep a b)] ++ reformatTableCaption n xs st
reformatTableCaption n ((Environment TableCap _ l) : xs) st
  = if (filter (not . isSpace) (treeToLaTeX l st)) == [] then
      reformatTableCaption n xs st else
      (Environment TableRowSep (Str "") []) :
        ((Environment TableHeadColSep (Str "")
            [Environment Attribute (Attr ("colspan", (show n))) []])
           : l)
          ++
          [(Environment TableRowSep (Str "") [])] ++
            reformatTableCaption n xs st
reformatTableCaption n (x : xs) st
  = x : reformatTableCaption n xs st
reformatTableCaption _ [] _ = []

{-DHUN| In order to determine the maximum width of columns, each table is precompiled with latex several times, with only one column included each time. this function creates the list of the latex sources of these tables, for one table in the parse tree DHUN-}

maketablist ::
            [Anything Char] -> TableState -> Int -> MyState -> [[Char]]
maketablist l tst nc mst = map tablo [1 .. (nc + 1)]
  where tablo n
          = "\\begin{tabular}{|" ++
              (replicate nc 'l') ++
                "|}" ++
                  "\\begin{varwidth}{\\linewidth}" ++
                    (fst
                       (fst
                          (runState
                             ((runStateT (tableContentToLaTeX2 l))
                                tst{inputLastRowOfHeader = -2, activeColumn = Just n})
                             mst)))
                      ++ "\\end{tabular}"

{-DHUN| Takes a map from int to double finds the biggest double and removes the corresponding key value pair from the map. This way wide columns are set to smaller sizes in order to fit the whole table onto the page width DHUN-}

removehighest :: Map Int Double -> Map Int Double
removehighest m
  | m /= Map.empty = Map.fromList (hlp (Map.toList m))
  where mx = maximum (Map.elems m)
        hlp ((_, v) : xs) | v == mx = xs
        hlp (x : xs) = x : (hlp xs)
        hlp [] = []
removehighest _ = Map.empty

{-DHUN| Returns a list of floats which represents the width of the columns of a table in units of the line width with the proper corrections for use in the a latex documents. If the boolean input parameter is true the table is understood to be written in landscape mode. It also take a map of Int to Double. This is the list of the maximum width of columns determined by  previous runs of latex on the table with only one column included per run DHUN-}

wdth3 :: Bool -> Map Int Double -> ([Float], Double)
wdth3 ls m
  | m /= Map.empty =
    ((map
        ((* (1.0 - (scalefactor (fromIntegral n)))) .
           double2Float . (/ (linew2 ls)))
        (Map.elems mm)),
     if d == 1.0 then 1.0 else d * 0.9)
  where n = (maximum (Map.keys m))
        (mm, d) = wdth ls n m
wdth3 _ _ = ([], 1.0)

{-DHUN| Returns a table header which represents the width of the columns of a table in units of the line width with the proper corrections for use in the a latex documents. If the first boolean input parameter is true the table is understood to be written in landscape mode. It also take a map of Int to Double. This is the list of the maximum width of columns determined by  previous runs of latex on the table with only one column included per run. If second boolean parameter is true it is understood the the rule should be printed with the table, otherwise the table should be printed without rules DHUN-}

wdth2 :: Bool -> Map Int Double -> Bool -> Float -> String
wdth2 ls m b f
  | m /= Map.empty =
    tableSpecifier b
      (map
         ((* (f*(1.0 - (scalefactor (fromIntegral n))))) .
            double2Float . (/ (linew2 ls)))
         (Map.elems mm))
  where n = (maximum (Map.keys m))
        (mm, _) = wdth ls n m
wdth2 _ _ _ _ = []

{-DHUN| takes the list of maximum column widths created by previous runs of the latex compiler with only one columns included per run as map from Int to Double. Take the total number of columns of the table as Int. The table is understood to be printed in landscape mode if the boolean parameter is true. It returns a map from int to double representing the width of columns of the table to be used in the latex documents. So it takes raw widths. Which are just the width of the column if the width of the paper was infinite and return the width that fit on the finite width of the real paper DHUN-}

wdth :: Bool -> Int -> Map Int Double -> (Map Int Double, Double)
wdth ls n mm
  = case
      (Control.Monad.msum
         (map hlp
            (zip
               (iterate removehighest
                  (Map.mapMaybe
                     (\ x -> Just $ x / (1.0 - (scalefactor (fromIntegral n))))
                     mm))
               [0 .. (length (Map.keys mm))])))
      of
        Just (x, Nothing) -> if (sum (Map.elems x)) < (linew2 ls) then
                               (Map.map (\ y -> y * ((linew2 ls) / (sum (Map.elems x)))) x, 1.0)
                               else (x, 1.0)
        Just (x, Just ddd) -> (Map.map
                                 (\ y -> y * ((linew2 ls) / (sum (Map.elems x))))
                                 x,
                               ddd)
        Nothing -> (myfill ((linew2 ls) / (fromIntegral n)) Map.empty, 1.0)
  where hlp ::
            (Map Int Double, Int) -> Maybe ((Map Int Double), Maybe Double)
        hlp (m, i)
          | ((sum (Map.elems m)) :: Double) +
              ((linew2 ls) / (fromIntegral n)) * (fromIntegral i)
              < (linew2 ls)
            =
            Just
              (myfill
                 (((linew2 ls) - (sum (Map.elems m))) /
                    ((fromIntegral i) :: Double))
                 m,
               Nothing)
        hlp (_, i)
          | ((i == 3) || (i == n)) =
            let (mmm, dd) = (wdth ls n (Map.map ((\ x -> x * 0.95)) mm)) in
              Just (mmm, Just ((0.965) * dd))
        hlp (m, i)
          | ((sum (Map.elems m)) :: Double) +
              ((linew2 ls) / (fromIntegral n)) * (fromIntegral i)
              >= (linew2 ls)
            = Nothing
        hlp _
          = Just (myfill ((linew2 ls) / (fromIntegral n)) Map.empty, Nothing)
        
        myfill :: Double -> Map Int Double -> Map Int Double
        myfill x m = Map.union m (Map.fromList (zip [1 .. n] (repeat x)))

{-DHUN| In landscape mode everything has to be multiplied by a factor of two. If the boolean parameter is true it is understood that the table should be printed in landscape mode. This function return the width of the line in latex using the units of latex DHUN-}

linew2 :: Bool -> Double
linew2 ls = if ls then linew * 1.414 else linew

{-DHUN| The width of the line in A4 paper with DIV margin factor of 13 in latex own units DHUN-}

linew :: Double
linew = 455.45742

{-DHUN| convert a table form the parse tree to latex. The [Anything Char] parameter it the contend of the table represented as a parse tree. The String parameter contains the HTML attributes of the table element, or in wiki notation the HTML parameters of the line beginning with  {| . This is evaluated in order to find out whether rules should be printed in the table. The return type is Renderer String. Which means that it returns a string but also take a state as additional monadic input parameter and returns a possible changed version of it as additional return parameter monadically DHUN-}

tableToLaTeX :: [Anything Char] -> Bool -> String -> Maybe Float -> Renderer String
tableToLaTeX l1 b s m
  = do st <- get
       let modst = st{getF = (getF st) * (tableScale (numberOfColumns l))}
           ((_, oldstate), _)
             = runState ((runStateT (tableContentToLaTeX2 reformed)) tblstate)
                 modst
           ((t1, _), newstate)
             = runState
                 ((runStateT (tableContentToLaTeX2 reformed))
                    tblstate{inputLastRowOfHeader =
                               if outputTableHasHeaderRows oldstate then
                                 outputLastRowOfHeader oldstate else -2})
                 modst{tablist =
                         (maketablist reformed tblstate (numberOfColumns l) modst) :
                           (tablist st)}
           reformed = ((reformatTableCaption (numberOfColumns l) l st))
           l = l1 --stripempty l1 st
           spec
             = case Map.lookup tbno (tabmap st) of
                   Nothing -> (if (tableSpecifier sep widths) == "" then
                                 "p{\\linewidth}" else tableSpecifier sep widths)
                   Just t -> wdth2 lsc (Map.map ((if b then 0.5 else 1.0)*) t) sep (if b then 0.5 else 1.0)
           sep = seperatingLinesRequested s
           hline = horizontalLine sep
           widths = case m of
                      Just d -> if sum basewidths > d / 400.0 then map ((d/400.0)*) basewidths else basewidths
                      Nothing -> basewidths 
           (basewidths, fontscalefactor)
             = case Map.lookup tbno (tabmap st) of
                   Nothing -> (columnWidths l, 1.0)
                   Just t -> wdth3 lsc t
           env = tableEnvironment (getF st)
           scriptsize
             = (isInfixOf2 "latexfontsize=\"scriptsize\"" s) ||
                 ((numberOfColumns l) > 5)
           sb = if scriptsize then "{\\scriptsize{}" else ""
           se = if scriptsize then "}" else ""
           lsc
             = (env == "longtable") &&
                 (((numberOfColumns l) > 100))
           lsb = (if lsc then "\\begin{landscape}\n" else "")
           lse = (if lsc then "\n\\end{landscape}" else "")
           tbno = (length (tablist st)) + 1
           tblstate
             = TableState{seperatingLinesRequestedForTable = sep,
                          lastCellWasNotFirstCellOfRow = False,
                          lastCellWasMultiColumn = False, columnsWidthList = widths,
                          currentColumn = 1, multiRowMap = Map.empty,
                          lastCellWasMultiRow = False,
                          numberOfColumnsInTable = (numberOfColumns l), isFirstRow = True,
                          lastCellWasHeaderCell = False, currentRowIsHeaderRow = False,
                          stillInTableHeader = True, rowCounter = 0,
                          outputLastRowOfHeader = 0, inputLastRowOfHeader = 0,
                          lastRowHadEmptyMultiRowMap = True,
                          outputTableHasHeaderRows = False, activeColumn = Nothing}
       put $ newstate{getF = getF st}
       r <- return $
              lsb ++
                sb ++
                  (if (env /= "tabular") then "\n" else "\\scalebox{0.85}{") ++
                    (if fontscalefactor == 1.0 then "" else
                       "{\\scalefont{" ++ (printf "%0.5f" fontscalefactor) ++ "}")
                      ++
                      "\\begin{" ++
                        env ++
                          "}{" ++
                            spec ++
                              "}" ++
                                hline ++
                                  " \n" ++
                                    t1 ++
                                      (rowDelimiter sep) ++
                                        " \n\\end{" ++
                                          env ++
                                            "}\n" ++
                                              (if fontscalefactor == 1.0 then "" else "}") ++
                                                (if (env /= "tabular") then "" else "}") ++
                                                  se ++ lse ++ (if (env == "tabular") then " " else "")
       return r

{-DHUN| Converts an image from the parse tree to latex. The actual images is only referenced in the wiki source, as well as the parse tree, as well as the latex source. It takes a parse tree representation of the image as only input parameter. The return type is Renderer String. Which means that it returns a string but also take a state as additional monadic input parameter and returns a possible changed version of it as additional return parameter monadically DHUN-}

wikiImageToLaTeX :: [Anything Char] -> Renderer String
wikiImageToLaTeX l
  = do st <- get
       mystr <- return $
                  (if not (micro st) then
                     "\n" ++
                       (if ((getInTab st) == 0) then "\n" else "") ++
                         "\\begin{minipage}{" ++
                           (if (msb st) then "1.0" else (mysize st)) ++
                             (if (msb st) then "\\linewidth" else "\\textwidth") ++ "}\n"
                     else (if ((getInTab st) == 0) then "\n" else ""))
                    ++
                    (if (not (micro st)) then "\\begin{center}\n" else "") ++
                      "\\includegraphics[width=" ++
                        (if (not (micro st)) then "1.0" else (mysize st)) ++
                          (if (msb st) then "\\linewidth" else "\\textwidth") ++
                            ",height=6.5in,keepaspectratio]{../images/" ++
                              (n st) ++
                                "." ++
                                  ext ++
                                    "}\n" ++
                                      (if (not (micro st)) then "\\end{center}\n" else "") ++
                                        (if (not (tb st)) && (not (micro st)) then "\\raggedright{}"
                                           else "")
                                          ++
                                          (if not (micro st) then
                                             (if (s st) == "" then
                                                "\\myfigurewithoutcaption{" ++ (n st) ++ "}" else
                                                "\\myfigurewithcaption{" ++
                                                  (n st) ++ "}{" ++ (s st) ++ "}")
                                             else "")
                                            ++
                                            (if not (micro st) then "\n\\end{minipage}" else "") ++
                                              (addit st) ++
                                                (if not (micro st) then
                                                   (if ((getInTab st) == 0) then "\n" else "") ++
                                                     "\n"
                                                   else " ")
       put
         st{getImages = (getImages st) ++ [shallowFlatten l],
            getJ = ((getJ st) + 1)}
       return mystr
  where ext
          = normalizeExtension
              (map toLower
                 (fileNameToExtension (headSplitEq '|' (shallowFlatten l))))
        s st
          = if (trim (s1 st)) `elem` ["verweis=", "alt=", "link="] then ""
              else (s1 st)
        s2 st
          = case Map.lookup "alt" (snd (prepateTemplate l "x")) of
                Just xx -> wikiLinkCaption xx st
                Nothing -> wikiLinkCaption l st
        s1 st
          = if '|' `elem` (shallowFlatten l) then (s2 st) else
              (treeToLaTeX [] st{getJ = ((getJ st) + 1)})
        mysize st = printf "%0.5f" (mysizefloat2 st)
        mysizefloat st = (min (getF st) (imageSize l))
        mysizefloat2 st = if (msb st) then 1.0 else (mysizefloat st)
        msb st = (mysizefloat st) == (getF st)
        micro st = ((mysizefloat st) < 0.17) || ((getInTab st) > 1)
        n st = show (getJ st)
        tb st = ((getInTab st) > 0)
        addit st
          = if (getInTab st) > 0 then "" else
              (if not (micro st) then "\\vspace{0.75cm}" else "")

{-DHUN| Returns the caption of a wikilink. Takes a parse tree representation of the wikilink and the current state of the renderer. Return the caption in LaTeX representation as string. A Wikilink is represented as [[FooBar]] in Wiki notation. DHUN-}

wikiLinkCaption :: [Anything Char] -> MyState -> String
wikiLinkCaption l st = if isCaption x then rebuild x else ""
  where x = (treeToLaTeX (last (splitOn [C '|'] l)) st{getInCaption=True})
        rebuild (':' : xs) = xs
        rebuild b = b

{-DHUN| Returns the LaTeX representation of a wikilink. Takes a parse tree representation of the wikilink and the current state of the render. A Wikilink is represented as [[FooBar]] in Wiki notation. DHUN-}

wikiLinkToLaTeX :: [Anything Char] -> MyState -> String
wikiLinkToLaTeX l st
  = case
      Map.lookup (map toUpper (finalloc st))
        (Map.mapKeys (map toUpper) (urls st))
      of
        Just yy -> "\\my" ++
                     addit ++ "lref{" ++ yy ++ "}{" ++ (wikiLinkCaption l st) ++ "}"
        Nothing -> case
                     do hh <- maybeHead . (splitOn "#") . (map toUpper) $ (finalloc st)
                        Map.lookup (Just hh)
                          (Map.mapKeys (maybeHead . (splitOn "#") . (map toUpper)) (urls st))
                     of
                       Just yy -> "\\my" ++
                                    addit ++ "lref{" ++ yy ++ "}{" ++ (wikiLinkCaption l st) ++ "}"
                       Nothing -> "\\my" ++
                                    addit ++
                                      "href{" ++
                                        (wikiLinkLocationesc l st) ++
                                          "}{" ++ (killnl (wikiLinkCaption l st)) ++ "}"
  where zzz sssst
          = case localWikiLinkLocation (loc) of
                ('#' : xs) -> (currentUrl sssst) ++ ('#' : xs)
                xs -> xs
        finalloc3 sts = replace2 (trim (zzz sts)) " " "_"
        finalloc ssst
          = case reverse (finalloc3 ssst) of
                ('/' : xs) -> reverse xs
                _ -> finalloc3 ssst
        restpath
          = intercalate "/"
              (reverse (drop len (reverse (splitOn "/" (currentUrl st)))))
        loc = if len > 0 then restpath ++ "/" ++ rest else rest
        (len, rest) = doit2 0 (shallowFlatten l)
        doit2 n ('.' : ('.' : ('/' : xs))) = doit2 (n + 1) xs
        doit2 n xs = (n, xs)
        addit
          = if getInFootnote st then "fn" else
              if ((getInTab st) > 1) then "tab" else ""
        killnl ('\n' : ('\n' : xs)) = killnl ('\n' : xs)
        killnl (x : xs) = x : (killnl xs)
        killnl [] = []

{-DHUN| If repeated newlines appear in a string directly after each other. Each series of newlines is reduced to exactly one newline DHUN-}

killnl2 :: String -> String
killnl2 ('\n' : ('\n' : xs)) = killnl2 ('\n' : xs)
killnl2 ('\n' : xs)
  = if (trim pre) == "" then killnl2 post else pre ++ (killnl2 post)
  where pre = (takeWhile (/= '\n') xs)
        post = (dropWhile (/= '\n') xs)
killnl2 (x : xs) = x : (killnl2 xs)
killnl2 [] = []

{-DHUN| returns the caption of a link. A link is represented as [foobar.com mycaption] in wiki notation. It takes the parse tree representation of the link as first input parameter. The second input parameter is the current state of the renderer. The third parameter is the Uri scheme as string (See 'URI scheme' in the English wikipeda) usually this is 'http'. It returns the latex representation of the caption of the link as string DHUN-}

linkCaption ::
            [Anything Char] -> MyState -> String -> Bool -> String
linkCaption l st s b
  = case spl of
        (_ : (gg : gs)) -> (treeToLaTeX
                              (concat (gg : (map (\ x -> (C ' ') : x) gs)))
                              st)
        _ -> if b then "" else s ++ (escapelink (linkLocation l))
  where spl = splitOn [C ' '] l

{-DHUN| returns the latex representation of a link. A link is represented as [foobar.com mycaption] in wiki notation. It takes the parse tree representation of the link as first input parameter. The second input parameter is the current state of the renderer. The third parameter is the Uri scheme as string (See 'URI scheme' in the English wikipeda) usually this is 'http'. It returns the latex representation of the link as string DHUN-}

linkToLaTeX :: [Anything Char] -> MyState -> String -> String
linkToLaTeX l st s
  = if
      (b && cap == "") || (cap == (s ++ (escapelink (linkLocation l))))
      then "\\myplainurl{" ++ s ++ (escapelink (linkLocation l)) ++ "}"
      else
      "\\my" ++
        addit ++
          "href{" ++ s ++ (escapelink (linkLocation l)) ++ "}{" ++ cap ++ "}"
  where addit = if b then "fn" else ""
        b = getInFootnote st
        cap = (linkCaption l st s b)

{-DHUN| takes a list and splits it into sublist of equal length, allowing a possible smaller length for the last list in case the devision does not create an integer result. DHUN-}

splitToTuples :: [a] -> [[a]]
splitToTuples x
  = map (take galleryNumberOfColumns) .
      takeWhile (not . null) . iterate (drop galleryNumberOfColumns)
      $ x

{-DHUN| the number of column to be used in latex documents for mediawikis gallery (image gallery) (gallery tags) DHUN-}

galleryNumberOfColumns :: Int
galleryNumberOfColumns = 1

{-DHUN| the width of a column for the table of the latex version of mediawikis gallery (image gallery, gallery tags) DHUN-}

galleryTableScale :: Float
galleryTableScale
  = (1.0 / (fromIntegral galleryNumberOfColumns)) - (scalefactor 1.0)

{-DHUN| the latex string for a single column in table header in the latex version of mediawikis gallery (image gallery, gallery tag) DHUN-}

galleryTableSpecifier :: String
galleryTableSpecifier
  = concat $
      replicate galleryNumberOfColumns
        ">{\\RaggedRight}p{0.5\\linewidth}"

{-DHUN| converts the inner parts gallery (image gallery, gallery tag) from parse tree notation to latex, does not write the latex table header and footer. This is only a helper function. Always use galleryToLatex if you want to convert a gallery to latex DHUN-}

galleryContentToLatex :: [[Anything Char]] -> Renderer String
galleryContentToLatex (x : xs)
  = do s <- galleryRowToLaTex x
       ss <- galleryContentToLatex xs
       return $ s ++ "\\\\ \n" ++ ss
galleryContentToLatex [] = return []

{-DHUN| converts a part of a gallery (image gallery, gallery tag) from parse tree to latex. A part are as many elements as fit into a single row in the resulting latex table DHUN-}

galleryRowToLaTex :: [Anything Char] -> Renderer String
galleryRowToLaTex [] = return []
galleryRowToLaTex (x : []) = treeToLaTeX2 [x]
galleryRowToLaTex (x : xs)
  = do s <- treeToLaTeX2 [x]
       g <- galleryRowToLaTex xs
       return $ s ++ "&" ++ g

{-DHUN| Converts are gallery (image gallery, gallery tag) from parse tree to latex. Also writes table header and footer. This is the function you should use for converting galleries to latex DHUN-}

galleryToLatex :: [Anything Char] -> Renderer String
galleryToLatex x
  = do st <- get
       put st{getF = (getF st) * galleryTableScale}
       s <- (galleryContentToLatex
               [z | z <- splitToTuples [y | y <- x, isWikiLink y],
                trim (treeToLaTeX z st) /= ""])
       st2 <- get
       put st2{getF = (getF st)}
       return
         ("\\begin{longtable}{" ++
            galleryTableSpecifier ++ "}  \n" ++ s ++ "\\end{longtable}")

{-DHUN| A function to drop all unnecessary elements of an HTML image map, so that it can be converted  to latex by calling treeToLaTeX2 DHUN-}

imageMapClean :: [Anything Char] -> [Anything Char]
imageMapClean ((Environment Wikilink s l) : xs)
  = (Environment Wikilink s l) : imageMapClean xs
imageMapClean (_ : xs) = imageMapClean xs
imageMapClean [] = []

{-DHUN| Takes the parse tree representation of an image, and returns the its size in unit of the width of a line in latex. Images 400px or wider in wiki notation are understood to use the full width of the line. Smaller one are considered fractionally. That means 100px means 0.25 the width of the line and 200px means 0.5 width of the line DHUN-}

imageSize :: [Anything Char] -> Float
imageSize l = if [] == x then 1.0 else (minimum x)
  where x = map readImageSize (imageSizeStrings (shallowFlatten l))

{-DHUN| takes a candidate string for the width of an image in the wikis px notation. Returns 1.0 if the candidate could not be parsed, returns the width of the image in units of the width of a line in latex otherwise. Images 400px or wider in wiki notation are understood to use the full width of the line. Smaller one are considered fractionally. That means 100px means 0.25 the width of the line and 200px means 0.5 width of the line DHUN-}

readImageSize :: String -> Float
readImageSize y
  = case (reads x) of
        [] -> 1.0
        (h : _) -> fst h / 400.0
  where x = removex y
        removex ('x' : zs) = zs
        removex z = z

{-DHUN| takes a flattend version of a parse tree represendtation of an image and retruns a list of substrings which are candidates for representing the width of the image in the wikis px notation DHUN-}

imageSizeStrings :: String -> [String]
imageSizeStrings s
  = [take (length (x) - 2) (x) |
     x <- ((splitOn ['|'] s) :: [String]), isSuffixOf "px" x]

{-DHUN| converts a mathematical fomula from the wiki to latex notation DHUN-}

mathToLatex :: [Anything Char] -> String
mathToLatex l
  = if isInfixOf2 "\\begin{alignat}" (shallowFlatten l) then
      mathTransform l else "{$" ++ (mathTransform l) ++ "$}"

{-DHUN| a predicate that returns true if and only if the input is a parse tree that contains only spaces but no other structures DHUN-}

onlySpaces :: [Anything Char] -> Bool
onlySpaces ((C ' ') : xs) = onlySpaces xs
onlySpaces [] = True
onlySpaces _ = False

{-DHUN| in the wiki notation pipe (|)inside temples are escaped as {!} and double pipes as {!!}}. this function undoes this escaping in a parse tree DHUN-}

prepateParameter :: [Anything Char] -> [Anything Char]
prepateParameter ((Environment Template _ [C '!']) : xs)
  = (C '|') : prepateParameter xs
prepateParameter ((Environment Template _ [C '!', C '!']) : xs)
  = (C '|') : (C '|') : prepateParameter xs
prepateParameter (x : xs) = x : prepateParameter xs
prepateParameter [] = []

{-DHUN| this function prepares a template as parse by the parser in the parse tree notation into a other notation that can be further processes by the latex renderer and the function templateToLatex in particular. The first input parameter is the parse tree notation of the template. The second is the name of the template as string. It returns a tuple, the first element of this tuple is the name of the template (stripped of heading an tailing white space) and the second element of the tuple is a map from strings to parse trees. The strings are the names of the parameters of the template. These might be just numbers represented as string but also any other strings DHUN-}

prepateTemplate ::
                [Anything Char] -> String -> (String, Map String [Anything Char])
prepateTemplate ll x = (trim x, enum ll 1 (Map.fromList []))
  where enum ::
             [Anything Char] ->
               Integer -> Map String [Anything Char] -> Map String [Anything Char]
        enum ((Environment TemplateInside (Str "") l) : zs) i d
          = enum zs (i + 1) (Map.insert (show i) (prepateParameter l) d)
        enum ((Environment TemplateInside (Str z) l) : zs) i d
          = enum zs i (Map.insert (trim z) (prepateParameter l) d)
        enum [] _ d = d
        enum (_ : zs) i d = enum zs i d

{-DHUN| converts a template from the wiki to latex. The first parameter is the parse tree representation of the template as generated by the parse the second is the name of the template. It returns a Renderer String. That is it returns the latex representation of the template, but also takes a state as an additional monadic input parameter and returns a possible changed version of it as additional return parameter monadically DHUN-}

templateToLatex :: [Anything Char] -> String -> Renderer String
templateToLatex l s
  = state $
      \ st ->
        case l of
            ((C 'w') : ((C '|') : xs)) -> (wikiLinkToLaTeX
                                             ((C 'w') : (C ':') : xs)
                                             st,
                                           st)
            ((C 'W') : ((C '|') : xs)) -> (wikiLinkToLaTeX
                                             ((C 'w') : (C ':') : xs)
                                             st,
                                           st)
            ((C 'B') : ((C '|') : xs)) -> (wikiLinkToLaTeX xs st, st)
            _ -> swap $ templateProcessor st (prepateTemplate l s)

mnfindentlatex :: Map String [Anything Char] -> Renderer String
mnfindentlatex ll
  = do one <- treeToLaTeX2 (Map.findWithDefault [] "1" ll)
       st <- get
       return
         ("\n\\begin{" ++
            (itemEnvironmentName ":" (getF st)) ++
              "}" ++
                (itemEnvironmentParameters ":" (getF st)) ++
                  "\n\\item{}" ++
                    one ++ "\n\\end{" ++ (itemEnvironmentName ":" (getF st)) ++ "}\n")

{-DHUN| function to converts wikipedias citearticle template to latex DHUN-}

citearticle :: Map String [Anything Char] -> Renderer String
citearticle ll
  = state $
      \ st ->
        ((treeToLaTeX (Map.findWithDefault [] "author" ll) st) ++
           (treeToLaTeX (Map.findWithDefault [] "first" ll) st) ++
             (if Map.member "first" ll then " " else "") ++
               (treeToLaTeX (Map.findWithDefault [] "last" ll) st) ++
                 ". " ++
                   (treeToLaTeX (Map.findWithDefault [] "title" ll) st) ++
                     (if Map.member "url" ll then
                        "\\my" ++
                          (if getInFootnote st then "fn" else "") ++
                            "href{" ++
                              (treeToLaTeX (deepFlatten (Map.findWithDefault [] "url" ll)) st) ++
                                "}{" ++ (treeToLaTeX (Map.findWithDefault [] "title" ll) st) ++ "}"
                        else (treeToLaTeX (Map.findWithDefault [] "title" ll) st))
                       ++
                       ". " ++
                         "\\textit{{}" ++
                           (treeToLaTeX (Map.findWithDefault [] "journal" ll) st) ++
                             "}, " ++
                               (if Map.member "volume" ll then
                                  "{{\\bfseries " ++
                                    (treeToLaTeX (Map.findWithDefault [] "volume" ll) st) ++ "}}"
                                  else "")
                                 ++
                                 (if Map.member "publisher" ll then
                                    "(" ++
                                      (treeToLaTeX (Map.findWithDefault [] "publisher" ll) st) ++
                                        ")"
                                    else "")
                                   ++
                                   (if Map.member "number" ll then
                                      "(" ++
                                        (treeToLaTeX (Map.findWithDefault [] "number" ll) st) ++ ")"
                                      else "")
                                     ++
                                     (if Map.member "pages" ll then
                                        ":" ++ (treeToLaTeX (Map.findWithDefault [] "pages" ll) st)
                                        else "")
                                       ++
                                       (if Map.member "month" ll then
                                          (treeToLaTeX (Map.findWithDefault [] "month" ll) st) else
                                          "")
                                         ++
                                         (treeToLaTeX (Map.findWithDefault [] "year" ll) st) ++
                                           (if Map.member "url" ll then
                                              (treeToLaTeX (Map.findWithDefault [] "url" ll) st)
                                              else "")
                                             ++ "\n",
         st)

{-DHUN| removes source structures from a parse tree, keeping the source inside the source structure in the parse tree, so flattens out the source structure. You need this if some parse tree contains source code but you don't know whether or not it is inside a source tag DHUN-}

flattensource :: [Anything Char] -> [Anything Char]
flattensource ((Environment Source (TagAttr _ _) l) : xs)
  = l ++ (flattensource xs)
flattensource (x : xs) = x : (flattensource xs)
flattensource [] = []

{-DHUN| prepare code for printing in latex. takes the current state of the renderer as first parameter. takes the map version of the template containing the code as second parameter. returns the latex representation as string DHUN-}

trilex :: MyState -> Map String [Anything Char] -> String
trilex st ll = trilexgen st ll "code"

{-DHUN| prepare code for printing in latex. Takes the map version of the template containing the code as first parameter. returns the latex representation as string. It returns a Render String so that it can access the current state of the renderer as additional monadic input parameter DHUN-}

trilex2 :: Map String [Anything Char] -> Renderer String
trilex2 ll
  = do st <- get
       return $ trilexgen st ll "code"

{-DHUN| prepare code for printing in latex. takes the current state of the renderer as first parameter. takes the map version of the template containing the code as second parameter. takes the name of the parameter containing the actual source in the map as third parameter. returns the latex representation as string DHUN-}

trilexgen ::
          MyState -> Map String [Anything Char] -> String -> String
trilexgen st ll code
  = if Map.member code ll then
      (treeToLaTeX
         (breakLines3 73
            (killnewline (flattensource (Map.findWithDefault [] code ll))))
         st)
      else ""
  where killnewline :: [Anything Char] -> [Anything Char]
        killnewline ((C '\n') : xs) = killnewline xs
        killnewline x = x

{-DHUN| analyzes a color in HTML notation. It returns a triple. The first element is a boolean. If it is true the color has got rgb hex notation and the third parameter contains the rgb latex notation. If it is false, the color is not rgb and hopefully a HTML color name, which is returned a second element of the tuple DHUN-}

colinfo :: String -> (Bool, String, String)
colinfo colcode
  = case col of
        Just colo -> (True, map toLower colname, colo)
        Nothing -> (False, map toLower colname,
                    case colnamehelper of
                        Just x -> x
                        Nothing -> colcode)
  where col = (makecol colnamehelper)
        colnamehelper
          = case colcode of
                (_ : (_ : gs)) -> Just gs
                _ -> Nothing
        colname
          = case
              do n <- colnamehelper
                 guard $ mypred n
                 return ("rgb" ++ n)
              of
                Just x -> x
                Nothing -> colcode
        
        ss :: String -> [Integer]
        ss (a : (b : xs)) = (maybeToList . unhex $ [a, b]) ++ (ss xs)
        ss _ = []
        
        ss3 :: String -> [Integer]
        ss3 (a : xs) = (maybeToList . unhex $ [a, a]) ++ (ss3 xs)
        ss3 _ = []
        
        ss2 :: [Integer] -> [Float]
        ss2 (x : xs) = ((fromInteger x) / 255.0) : ss2 xs
        ss2 [] = []
        ss4 x = if (((length . ss) x) == 3) then ss x else ss3 x
        
        prettyp2 :: [String] -> String
        prettyp2 (x : []) = x
        prettyp2 (x : xs) = x ++ "," ++ (prettyp2 xs)
        prettyp2 [] = []
        
        prettyp :: [String] -> String
        prettyp x = "{" ++ (prettyp2 x) ++ "}"
        
        makecol :: Maybe String -> Maybe String
        makecol x
          = do xx <- x
               guard $ mypred xx
               return $
                 prettyp ((map (printf "%0.5f") ((ss2 . ss4) xx)) :: [String])
        mypred x = (((length . ss) x) == 3) || (((length . ss3) x) == 3)

{-DHUN| and adapter to convert between the monadic and non monadic version of render. A function returning renderer string means that it returns a string but takes a state as additional monadic input parameter and returns the a possibly modified version of it as an additional monadic output parameter. This function takes a monadic return value. That is renderer String and returns its non monadic version. DHUN-}

tempProcAdapter ::
                Renderer String -> (MyState -> (MyState, String))
tempProcAdapter = (swap .) . runState

{-DHUN| function for key strokes templates in the blender wikibook DHUN-}

key :: [Char] -> [Char]
key "AKEY" = "A"
key "BKEY" = "B"
key "CKEY" = "C"
key "DKEY" = "D"
key "EKEY" = "E"
key "FKEY" = "F"
key "GKEY" = "G"
key "HKEY" = "H"
key "IKEY" = "I"
key "JKEY" = "J"
key "KKEY" = "K"
key "LKEY" = "L"
key "MKEY" = "M"
key "NKEY" = "N"
key "OKEY" = "O"
key "PKEY" = "P"
key "QKEY" = "Q"
key "RKEY" = "R"
key "SKEY" = "S"
key "TKEY" = "T"
key "UKEY" = "U"
key "VKEY" = "V"
key "WKEY" = "W"
key "XKEY" = "X"
key "YKEY" = "Y"
key "ZKEY" = "Z"
key "-KEY" = "-{}"
key "SEMICOLON" = ";"
key "NUM-" = "NUM-{}"
key x = x

{-DHUN| converts a template to latex. Takes the current state of the render as first input parameter. The second input parameter is a tuple. Its first element is the name of the template as string. Its second element is a map, mapping the names of the parameters of the template to their parse tree representations, it returns a tuple. The First element is the possible change state of the renderer the second one is the latex representation of the template DHUN-}

templateProcessor ::
                  MyState ->
                    (String, Map String [Anything Char]) -> (MyState, String)
templateProcessor st ("main", ll)
  = (st,
     "Main Page: " ++
       (wikiLinkToLaTeX (Map.findWithDefault [] "1" ll) st))
templateProcessor st ("#invoke:Mathe f\252r Nicht-Freaks/Seite", _)
  = (st, "")
templateProcessor st ("#invoke:Liste", _) = (st, "")
templateProcessor st ("!", _) = (st, "|")
templateProcessor st ("!!", _) = (st, "||")
templateProcessor st
  ("Mathe f\252r Nicht-Freaks: Vorlage:Frage", ll) = (st2, r)
  where j1 x = (Map.findWithDefault [] x ll)
        (r, st2)
          = runState
              (do a <- treeToLaTeX2 (j1 "typ")
                  b <- treeToLaTeX2 (j1 "frage")
                  c <- treeToLaTeX2 (j1 "antwort")
                  return $
                    if Map.member "typ" ll then
                      "\\NFFrage{" ++ a ++ "}{" ++ b ++ "}{" ++ c ++ "}" else
                      "\\NFFrageB{" ++ b ++ "}{" ++ c ++ "}")
              st
templateProcessor st ("Aufgabensammlung: Vorlage:Frage", ll)
  = (st2, r)
  where j1 x = (Map.findWithDefault [] x ll)
        (r, st2)
          = runState
              (do a <- treeToLaTeX2 (j1 "typ")
                  b <- treeToLaTeX2 (j1 "frage")
                  c <- treeToLaTeX2 (j1 "antwort")
                  return $
                    if Map.member "typ" ll then
                      "\\NFFrage{" ++ a ++ "}{" ++ b ++ "}{" ++ c ++ "}" else
                      "\\NFFrageB{" ++ b ++ "}{" ++ c ++ "}")
              st
templateProcessor st
  ("Mathe f\252r Nicht-Freaks: Vorlage:Mind Map", ll)
  = (st2,
     "\\section*{Mind Map}\\begin{landscape}" ++
       r ++ "\\end{landscape}")
  where (r, st2)
          = runState
              (wikiImageToLaTeX
                 ((C 'F') :
                    (C 'i') :
                      (C 'l') : (C 'e') : (C ':') : (Map.findWithDefault [] "1" ll)))
              st
templateProcessor st
  ("C++-Programmierung/ Vorlage:Buchinterner Link", ll)
  = (st,
     wikiLinkToLaTeX
       (if
          ("" /=
             (if length (splitOn "/" (currentUrl st)) > 2 then
                (id ((splitOn "/" (currentUrl st)) !! 2)) else []))
          then
          if Map.member "\220berschrift" ll then
            (map (C) "C++-Programmierung/ ") ++
              (Map.findWithDefault [] "Abschnitt" ll) ++
                [C '/', C ' '] ++
                  (Map.findWithDefault [] "Kapitel" ll) ++
                    [C '#'] ++
                      (Map.findWithDefault [] "\220berschrift" ll) ++
                        (if Map.member "Kapitelzusatz" ll then
                           ((C) '_') : (Map.findWithDefault [] "Kapitelzusatz" ll) else [])
            else
            (if Map.member "Kapitel" ll then
               (map (C) "C++-Programmierung/ ") ++
                 (Map.findWithDefault [] "Abschnitt" ll) ++
                   [C '/', C ' '] ++ (Map.findWithDefault [] "Kapitel" ll)
               else
               (map (C) "C++-Programmierung/ Inhaltsverzeichnis#Anker:") ++
                 (Map.findWithDefault [] "Abschnitt" ll))
              ++
              [C '|'] ++
                (Map.findWithDefault
                   (Map.findWithDefault
                      (Map.findWithDefault (Map.findWithDefault [] "Abschnitt" ll)
                         "Kapitel"
                         ll)
                      "\220berschrift"
                      ll)
                   "Text"
                   ll)
          else
          if Map.member "\220berschrift" ll then
            (map (C) "C++-Programmierung/ ") ++
              (Map.findWithDefault [] "Abschnitt" ll) ++
                [C '#'] ++
                  (Map.findWithDefault [] "\220berschrift" ll) ++
                    (if Map.member "Abschnittszusatz" ll then
                       ((C) '_') : (Map.findWithDefault [] "Abschnittszusatz" ll) else [])
            else
            (if Map.member "Kapitel" ll then
               (map (C) "C++-Programmierung/ ") ++
                 (Map.findWithDefault [] "Abschnitt" ll) ++
                   [C '#'] ++ (Map.findWithDefault [] "Kapitel" ll)
               else
               (map (C) "C++-Programmierung/ ") ++
                 (Map.findWithDefault [] "Abschnitt" ll))
              ++
              [C '|'] ++
                (Map.findWithDefault
                   (Map.findWithDefault
                      (Map.findWithDefault (Map.findWithDefault [] "Abschnitt" ll)
                         "Kapitel"
                         ll)
                      "\220berschrift"
                      ll)
                   "Text"
                   ll))
       st)
templateProcessor st ("B3D:N2P/Do", ll)
  = (st,
     intercalate "+"
       (filter (/= "\\keystroke{}")
          (map
             (\ x ->
                "\\keystroke{" ++
                  (key . (map toUpper))
                    (treeToLaTeX (Map.findWithDefault [] (show x) ll) st)
                    ++ "}")
             ([1, 2, 3, 4] :: [Integer]))))
templateProcessor st ("HaskellGHCiExample", ll)
  = (st,
     ("\\LaTeXDoubleBoxTemplate{Example:}{" ++
        (treeToLaTeX (Map.findWithDefault [] "1" ll) st) ++
          "\\newline " ++ trilexgen st ll "2" ++ "}\n"))
templateProcessor st ("Latex Index", ll)
  = (st,
     ("\\index{" ++
        (concatMap go (deepFlatten (Map.findWithDefault [] "1" ll))) ++
          "}"))
  where go (C x) = [x]
        go _ = []
templateProcessor st ("Komplexe Zahlen/ Vorlage:Formel", ll)
  = (st,
     (treeToLaTeX (shallowEnlargeMath (Map.findWithDefault [] "1" ll))
        st))
templateProcessor st ("Ada/95/RM", ll)
  = (st,
     (linkToLaTeX
        ((map C
            "http://www.adaic.org/resources/add_content/standards/95lrm/ARM_HTML/RM-")
           ++
           one ++
             (if has then [C '-'] else []) ++
               two ++
                 (map C ".html") ++
                   [C ' '] ++
                     (if has then one ++ [C '.'] ++ two else
                        (map C "Annex ") ++ one ++ [C ':'])
                       ++ [C ' '] ++ (Map.findWithDefault [] "title" ll))
        st
        ""))
  where one = (Map.findWithDefault [] "1" ll)
        two = (Map.findWithDefault [] "2" ll)
        has = Map.member "2" ll
templateProcessor st ("Ada/2005/RM", ll)
  = (st,
     (linkToLaTeX
        ((map C
            "http://www.adaic.org/resources/add_content/standards/05rm/html/RM-2-")
           ++
           one ++
             (if has then [C '-'] else []) ++
               two ++
                 (map C ".html") ++
                   [C ' '] ++
                     (if has then one ++ [C '.'] ++ two else
                        (map C "Annex ") ++ one ++ [C ':'])
                       ++ [C ' '] ++ (Map.findWithDefault [] "title" ll))
        st
        ""))
  where one = (Map.findWithDefault [] "1" ll)
        two = (Map.findWithDefault [] "2" ll)
        has = Map.member "2" ll
templateProcessor st ("Ada/2012/RM", ll)
  = (st,
     (linkToLaTeX
        ((map C "http://www.ada-auth.org/standards/12rm/html/RM-") ++
           one ++
             (if has then [C '-'] else []) ++
               two ++
                 (map C ".html") ++
                   [C ' '] ++
                     (if has then one ++ [C '.'] ++ two else
                        (map C "Annex ") ++ one ++ [C ':'])
                       ++ [C ' '] ++ (Map.findWithDefault [] "title" ll))
        st
        ""))
  where one = (Map.findWithDefault [] "1" ll)
        two = (Map.findWithDefault [] "2" ll)
        has = Map.member "2" ll
templateProcessor st ("Fortran:Vorlage: Pre1", ll)
  = (st,
     "{\\ttfamily {\\scriptsize " ++
       "\\newline{}" ++
         s1 ++
           "\\newline{}" ++
             s2 ++
               "\\newline{}" ++
                 (treeToLaTeX (Map.findWithDefault [] "1" ll) st) ++
                   "\\newline{}" ++
                     s2 ++ "\\newline{}" ++ s1 ++ "\\newline{}" ++ "}}\n")
  where s1
          = "0${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}.${\\ttfamily }${}|${\\ttfamily }${}${\\ttfamily }${}1${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}.${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}2${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}.${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}3${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}.${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}4${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}.${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}5${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}.${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}6${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}.${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}7${\\ttfamily }${}|${\\ttfamily }${}${\\ttfamily }${}.${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}${\\ttfamily }${}8"
        s2
          = "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
templateProcessor st ("Fortran:Vorlage: Pre2", ll)
  = (st,
     "{\\bfseries Fortran 90/95-Code (free source form) }\\newline" ++
       "{\\ttfamily {\\scriptsize " ++
         (treeToLaTeX (Map.findWithDefault [] "1" ll) st) ++ "}}\n")
templateProcessor st ("Fortran:Vorlage: Pre3", ll)
  = (st,
     "{\\bfseries Programmcode} \\newline" ++
       "{\\ttfamily {\\scriptsize " ++
         (treeToLaTeX (Map.findWithDefault [] "1" ll) st) ++ "}}\n")
templateProcessor st ("Fortran:Vorlage: Pre4", ll)
  = (st,
     "{\\bfseries Fortran 2003-Code} \\newline" ++
       "{\\ttfamily {\\scriptsize " ++
         (treeToLaTeX (Map.findWithDefault [] "1" ll) st) ++ "}}\n")
templateProcessor st ("Fortran:Vorlage: Intrinsic", ll)
  = (st,
     "\\newline{}\n" ++
       (treeToLaTeX (Map.findWithDefault [] "1" ll) st) ++
         " = {\\bfseries " ++
           (treeToLaTeX (Map.findWithDefault [] "2" ll) st) ++
             "} ( " ++
               (treeToLaTeX (Map.findWithDefault [] "3" ll) st) ++
                 " )\\newline{}\n")
templateProcessor st ("Fortran:Vorlage: IntrinsicS", ll)
  = (st,
     "\\newline{}\n {\\bfseries " ++
       (treeToLaTeX (Map.findWithDefault [] "1" ll) st) ++
         "} ( " ++
           (treeToLaTeX (Map.findWithDefault [] "2" ll) st) ++
             " )\\newline{}\n")
templateProcessor st ("Fortran:Vorlage: Isocbinding", ll)
  = (st,
     "Beispiel funktioniert mit Compiler\n" ++
       "\\begin{myitemize}\n" ++
         "\\item g95 (0.91!) May 10 2007: " ++
           (treeToLaTeX (Map.findWithDefault [] "1" ll) st) ++
             "\n" ++
               "\\item gfortran 4.3.0 20070723 (experimental): " ++
                 (treeToLaTeX (Map.findWithDefault [] "2" ll) st) ++
                   "\n" ++
                     "\\item Intel Fortran Compiler 10.0: " ++
                       (treeToLaTeX (Map.findWithDefault [] "3" ll) st) ++
                         "\n" ++
                           "\\item Sun Studio Express - June 2007: " ++
                             (treeToLaTeX (Map.findWithDefault [] "4" ll) st) ++
                               "\n" ++
                                 "\\end{myitemize}\n" ++
                                   "Anmerkungen:\\newline{}\n" ++
                                     (treeToLaTeX (Map.findWithDefault [] "5" ll) st) ++ "\n")
templateProcessor st ("C++-Programmierung/ Vorlage:Aufgabe", ll)
  = (st,
     ("{\\bfseries Aufgabe " ++
        (treeToLaTeX (Map.findWithDefault [] "Nummer" ll) st) ++
          "} \n" ++
            (treeToLaTeX (Map.findWithDefault [] "Aufgabe" ll) st) ++
              "\n {\\bfseries Musterl\246sung} \n" ++
                (treeToLaTeX (Map.findWithDefault [] "L\246sung" ll) st) ++ " \n"))
templateProcessor st ("-", ll)
  = (tempProcAdapter $ mnfindentlatex ll) st
templateProcessor st ("Haskell speaker 2", ll) = (st, param "1")
  where param n = (treeToLaTeX (Map.findWithDefault [] n ll) st)
templateProcessor st ("Vorlage:LaTeX Mehrspaltig Anfang", ll)
  = (st, "\\begin{multicols}{" ++ (param "1") ++ "}")
  where param n = (treeToLaTeX (Map.findWithDefault [] n ll) st)
templateProcessor st ("Vorlage:LaTeX Mehrspaltig Ende", _)
  = (st, "\\end{multicols}")
templateProcessor st ("Vorlage:Referenzbox IntraBuch", ll)
  = (st,
     "{\\bfseries Querverweise:} Siehe auch " ++
       (treeToLaTeX (Map.findWithDefault [] "1" ll) st))
templateProcessor st ("Referenzbox IntraBuch", ll)
  = (st,
     "{\\bfseries Querverweise:} Siehe auch " ++
       (treeToLaTeX (Map.findWithDefault [] "1" ll) st))
templateProcessor st ("Referenzbox Internet", ll)
  = (st,
     "{\\bfseries Themenbezogene} " ++
       (treeToLaTeX
          [Environment Wikilink (Str "")
             ((Map.findWithDefault [] "1" ll) ++ (map C "|Webangebote"))]
          st))
templateProcessor st ("Vorlage:Referenzbox Internet", ll)
  = (st,
     "{\\bfseries Themenbezogene} " ++
       (treeToLaTeX
          [Environment Wikilink (Str "")
             ((Map.findWithDefault [] "1" ll) ++ (map C "|Webangebote"))]
          st))
templateProcessor st ("Referenzbox IntraReihe", ll)
  = (st,
     "{\\bfseries Zum anderen Band der " ++
       (treeToLaTeX
          [Environment Wikilink (Str "")
             ((Map.findWithDefault [] "1" ll) ++
                [C '|'] ++ (Map.findWithDefault [] "2" ll))]
          st)
         ++ "}" ++ (treeToLaTeX (Map.findWithDefault [] "3" ll) st))
templateProcessor st ("Vorlage:Referenzbox IntraReihe", ll)
  = (st,
     "{\\bfseries Zum anderen Band der " ++
       (treeToLaTeX
          [Environment Wikilink (Str "")
             ((Map.findWithDefault [] "1" ll) ++
                [C '|'] ++ (Map.findWithDefault [] "2" ll))]
          st)
         ++ "}" ++ (treeToLaTeX (Map.findWithDefault [] "3" ll) st))
templateProcessor st ("unicode", ll)
  = (st, (treeToLaTeX (Map.findWithDefault [] "1" ll) st))
templateProcessor st
  ("Praktikum Anorganische Chemie/ Vorlage:Infobox Nachweisreaktion",
   ll)
  = (st,
     "\\begin{tabular}{|p{0.3\\linewidth}|p{0.7\\linewidth}|}\\hline\n"
       ++
       "\\multicolumn{2}{|p{1.0\\linewidth}|}{{\\bfseries Nachweisreaktion}} \\\\ \\hline\n"
         ++
         "Reaktionstyp & " ++
           (treeToLaTeX (Map.findWithDefault [] "Typ" ll) st) ++
             " \\\\ \\hline\n" ++
               "pH & " ++
                 (treeToLaTeX (Map.findWithDefault [] "pH" ll) st) ++
                   "\\\\ \\hline\n" ++
                     "Indikation & " ++
                       (treeToLaTeX (Map.findWithDefault [] "Indikation" ll) st) ++
                         "\\\\ \\hline \n\\end{tabular}\n")
templateProcessor st
  ("Praktikum Anorganische Chemie/ Vorlage:Gift", _)
  = (st, "{\\bfseries Gefahrstoffwarnung! $\\skull$ }")
templateProcessor st ("Wikibooks", ll)
  = (st, wikiLinkToLaTeX ((Map.findWithDefault [] "1" ll)) st)
templateProcessor st ("See also", ll)
  = (st,
     "See also: " ++
       (wikiLinkToLaTeX ((Map.findWithDefault [] "1" ll)) st))
templateProcessor st ("info", ll)
  = (st,
     "\\begin{TemplateInfo}{}{}" ++
       (treeToLaTeX ((Map.findWithDefault [] "1" ll)) st) ++
         "\\end{TemplateInfo}")
templateProcessor st ("Java:statement1", _)
  = (st, "statement{$_{\\textrm{\\scriptsize 1}}$}")
templateProcessor st ("Java:statement2", _)
  = (st, "statement{$_{\\textrm{\\scriptsize 2}}$}")
templateProcessor st ("Java:boolean-condition", _)
  = (st, "boolean-condition")
templateProcessor st ("Java:ch", ll)
  = (st,
     "'" ++ (treeToLaTeX ((Map.findWithDefault [] "1" ll)) st) ++ "'")
templateProcessor st ("Ubung", _) = (st, "\\ubung")
templateProcessor st ("TickYes", _) = (st, "\\TickYes")
templateProcessor st ("Achtung", ll)
  = (st,
     "\\begin{TemplateInfo}{\\danger}{Achtung}" ++
       (treeToLaTeX ((Map.findWithDefault [] "1" ll)) st) ++
         "\\end{TemplateInfo}")
templateProcessor st ("Warning", ll)
  = (st,
     "\\begin{TemplateInfo}{\\danger}{Warning}" ++
       (treeToLaTeX ((Map.findWithDefault [] "1" ll)) st) ++
         "\\end{TemplateInfo}")
templateProcessor st ("warning", ll)
  = (st,
     "\\begin{TemplateInfo}{\\danger}{Warning}" ++
       (treeToLaTeX ((Map.findWithDefault [] "1" ll)) st) ++
         "\\end{TemplateInfo}")
templateProcessor st ("Notiz", ll)
  = (st,
     "\\begin{TemplateInfo}{}{Notiz}" ++
       (treeToLaTeX ((Map.findWithDefault [] "1" ll)) st) ++
         "\\end{TemplateInfo}")
templateProcessor st ("Vorlage:Zitat3", ll)
  = (st,
     "\\begin{longtable}{|p{\\linewidth}|}\\hline\n \\uline{" ++
       (treeToLaTeX ((Map.findWithDefault [] "autor" ll)) st) ++
         "}\\\\\\textit{\n" ++
           (treeToLaTeX ((Map.findWithDefault [] "zitat" ll)) st) ++
             "}\\scriptsize \\\\ \\RaggedLeft \\scriptsize \n" ++
               (treeToLaTeX ((Map.findWithDefault [] "quelle" ll)) st) ++
                 "\\\\ \\hline \n\\end{longtable}\n")
templateProcessor st ("C++-Programmierung/ Vorlage:Frage", ll)
  = (st,
     if Map.member "Aufgabe" ll then
       ("\\begin{longtable}{|p{\\linewidth}|}\\hline\n {\\bfseries Aufgabe "
          ++
          (treeToLaTeX (Map.findWithDefault [] "Nummer" ll) st) ++
            "} \\\\ \\hline\n" ++
              (treeToLaTeX (Map.findWithDefault [] "Aufgabe" ll) st) ++
                "\\\\ \\hline\n {\\bfseries Musterl\246sung} \\\\ \\hline\n" ++
                  (treeToLaTeX (Map.findWithDefault [] "L\246sung" ll) st) ++
                    "\\\\ \\hline \n\\end{longtable}\n")
       else
       ("\\begin{longtable}{|p{\\linewidth}|}\\hline\n {\\bfseries Frage "
          ++
          (treeToLaTeX (Map.findWithDefault [] "Nummer" ll) st) ++
            "} \\\\ \\hline\n" ++
              (treeToLaTeX (Map.findWithDefault [] "Frage" ll) st) ++
                "\\\\ \\hline\n {\\bfseries Antwort} \\\\ \\hline\n" ++
                  (treeToLaTeX (Map.findWithDefault [] "Antwort" ll) st) ++
                    "\\\\ \\hline \n\\end{longtable}\n"))
templateProcessor st ("merke", ll)
  = (st,
     "\\begin{TemplateInfo}{}{" ++
       heading ++
         "}" ++
           (treeToLaTeX ((Map.findWithDefault [] "1" ll)) st) ++
             "\\end{TemplateInfo}")
  where heading
          = if Map.member "info" ll then
              treeToLaTeX (Map.findWithDefault [] "info" ll) st else "Hinweis"
templateProcessor st ("Merke", ll)
  = (st,
     "\\begin{TemplateInfo}{}{" ++
       heading ++
         "}" ++
           (treeToLaTeX ((Map.findWithDefault [] "1" ll)) st) ++
             "\\end{TemplateInfo}")
  where heading
          = if Map.member "info" ll then
              treeToLaTeX (Map.findWithDefault [] "info" ll) st else "Hinweis"
templateProcessor st
  ("Wie_mein_Buch_auf_die_Welt_kommt/_Vorlage:Zitat", ll)
  = templateProcessor st ("Zitat", ll)
templateProcessor st ("Zitat", ll)
  = (st,
     "\\begin{longtable}{|p{\\linewidth}|}\\hline\n" ++
       (if Map.member "beschreibung" ll then
          (treeToLaTeX ((Map.findWithDefault [] "beschreibung" ll)) st) ++
            "\\\\ \\hline"
          else "")
         ++
         " \\uline{" ++
           (treeToLaTeX
              ((Map.findWithDefault (Map.findWithDefault [] "Autor" ll) "autor"
                  ll))
              st)
             ++
             "}\\\\\\textit{\n" ++
               (treeToLaTeX
                  ((Map.findWithDefault (Map.findWithDefault [] "Zitat" ll) "zitat"
                      ll))
                  st)
                 ++
                 "}\\scriptsize \\\\ \\RaggedLeft \\scriptsize \n" ++
                   (treeToLaTeX
                      ((Map.findWithDefault (Map.findWithDefault [] "Quelle" ll) "quelle"
                          ll))
                      st)
                     ++ "\\\\ \\hline \n\\end{longtable}\n")
templateProcessor st ("java web api", ll)
  = (st,
     "\\myhref{http://java.sun.com/javase/6/docs/api/" ++
       loc ++ "}{" ++ cap ++ "}")
  where loc :: String
        loc = (shallowFlatten (Map.findWithDefault [] "1" ll))
        
        cap :: String
        cap
          = "Java API: " ++ (treeToLaTeX (Map.findWithDefault [] "2" ll) st)
templateProcessor st
  ("Python_unter_Linux: Vorlagen:VorlageAusgabe", ll)
  = (st,
     ("\n{\\bfseries Ausgabe}\\\\\n{\\ttfamily \\scriptsize \n" ++
        ("user\\@localhost:\\~\\$" ++
           (treeToLaTeX
              (breakLines3 linewidth (Map.findWithDefault [] "1" ll))
              st)
             ++
             "\\newline " ++
               (treeToLaTeX
                  (id
                     (filter
                        (\ x ->
                           case x of
                               C '\n' -> False
                               (Environment Tag (TagAttr "br" _) _) -> True
                               _ -> True)
                        (Map.findWithDefault [] "2" ll)))
                  st))
          ++ "}\n"))
templateProcessor st ("Python_unter_Linux: Vorlagen:VorlageQZ", ll)
  = (st,
     ("{\\ttfamily \\uline{" ++
        (treeToLaTeX (breakLines3 100 (Map.findWithDefault [] "1" ll)) st)
          ++ "}}"))
templateProcessor st
  ("Python_unter_Linux:  Vorlagen:VorlageQZ", ll)
  = (st,
     ("{\\ttfamily \\uline{" ++
        (treeToLaTeX (breakLines3 100 (Map.findWithDefault [] "1" ll)) st)
          ++ "}}"))
templateProcessor st ("Python unter Linux: Vorlagen:VorlageQZ", ll)
  = (st,
     ("{\\ttfamily \\uline{" ++
        (treeToLaTeX (breakLines3 100 (Map.findWithDefault [] "1" ll)) st)
          ++ "}}"))
templateProcessor st ("C++-Programmierung/ Vorlage:Syntax", ll)
  = (st,
     "{\\ttfamily {\\scriptsize " ++
       (treeToLaTeX (breakLines3 96 (Map.findWithDefault [] "code" ll))
          st)
         ++ "}}\n")
templateProcessor st
  ("C++-Programmierung/ Vorlage:Kapitelanhang", ll)
  = (st,
     ((if Map.member "Zusammenfassung" ll then
         "\n {\\bfseries \\large Zusammenfassung} \n " ++
           (treeToLaTeX (Map.findWithDefault [] "Zusammenfassung" ll) st)
         else "")
        ++
        (if Map.member "Fragen" ll then
           "\n  {\\bfseries \\large Fragen} \n " ++
             (treeToLaTeX (Map.findWithDefault [] "Fragen" ll) st)
           else "")
          ++
          (if Map.member "Aufgaben" ll then
             "\n  {\\bfseries \\large Aufgaben} \n " ++
               (treeToLaTeX (Map.findWithDefault [] "Aufgaben" ll) st)
             else "")
            ++ " \n"))
templateProcessor st ("code:Output", ll)
  = (st,
     ("{{" ++
        (if (Map.member "1" ll) then
           "{ {" ++
             (treeToLaTeX (breakLines3 96 (Map.findWithDefault [] "1" ll)) st)
               ++ "}}"
           else "")
          ++
          "}}\n$\\text{ }$\\newline{}\n{\\bfseries Code}\\newline{}" ++
            (if (Map.member "2" ll) then
               "{\\ttfamily {\\scriptsize" ++
                 (treeToLaTeX (breakLines3 96 (Map.findWithDefault [] "2" ll)) st)
                   ++ "}}"
               else "")
              ++
              (if (Map.member "3" ll) then
                 "\n{\\bfseries Output}\\newline{}{\\ttfamily {\\scriptsize" ++
                   (treeToLaTeX (breakLines3 96 (Map.findWithDefault [] "3" ll)) st)
                     ++ "}}"
                 else "")))
templateProcessor st ("bcode:Example", ll)
  = (st,
     ("{\\bfseries Code}\\newline{}{\\ttfamily {\\scriptsize" ++
        (if (Map.member "1" ll) then
           "{\\ttfamily {\\scriptsize" ++
             (treeToLaTeX (breakLines3 96 (Map.findWithDefault [] "1" ll)) st)
               ++ "}}"
           else "")
          ++
          "}}\n" ++
            (if (Map.member "2" ll) then
               "{\\ttfamily {\\scriptsize" ++
                 (treeToLaTeX (breakLines3 96 (Map.findWithDefault [] "2" ll)) st)
                   ++ "}}"
               else "")
              ++
              (if (Map.member "3" ll) then
                 "{\\ttfamily {\\scriptsize" ++
                   (treeToLaTeX (breakLines3 96 (Map.findWithDefault [] "3" ll)) st)
                     ++ "}}"
                 else "")))
templateProcessor st ("cite web", ll) = (st, mainer)
  where mainer
          = "\\myhref{" ++
              url ++
                "}{" ++
                  title ++ "}. " ++ publisher ++ ". Retrieved " ++ accessdate ++ " "
        url = shallowFlatten (Map.findWithDefault [] "url" ll)
        publisher = treeToLaTeX (Map.findWithDefault [] "publisher" ll) st
        title = treeToLaTeX (Map.findWithDefault [] "title" ll) st
        accessdate
          = treeToLaTeX (Map.findWithDefault [] "accessdate" ll) st
templateProcessor st ("code", ll) = (st, mainer)
  where mainer :: String
        mainer
          = "\\TemplateCode{" ++
              header ++
                "}{" ++ "}{" ++ "}{" ++ "}{" ++ lang ++ "}{" ++ code ++ "}{}{}{}"
        
        header :: String
        header
          = if Map.member "header" ll then
              (treeToLaTeX (Map.findWithDefault [] "header" ll)
                 st{getInCode = True})
              else ""
        
        lang :: String
        lang
          = if Map.member "lang" ll then
              (treeToLaTeX (Map.findWithDefault [] "lang" ll)
                 st{getInCode = True})
                ++ " Source"
              else ""
        
        code :: String
        code = trilexgen st{getInCode = True} ll "source"
templateProcessor st ("Java_Code_File", ll) = (st, mainer)
  where mainer :: String
        mainer
          = "\\TemplateCode{" ++
              header ++
                "}{" ++ "}{" ++ "}{" ++ "}{" ++ lang ++ "}{" ++ code ++ "}{}{}{}"
        
        header :: String
        header
          = if Map.member "header" ll then
              (treeToLaTeX (Map.findWithDefault [] "header" ll)
                 st{getInCode = True})
              else ""
        
        lang :: String
        lang = if Map.member "lang" ll then "Java Source" else ""
        
        code :: String
        code = trilexgen st{getInCode = True} ll "source"
templateProcessor st ("Syntax", ll) = (st, mainer)
  where mainer :: String
        mainer
          = "\\begin{TemplateCodeInside}{}{\\baselineskip}{\\baselineskip}{}{}{}\n"
              ++ code ++ "\n\\end{TemplateCodeInside}\n"
        
        code :: String
        code = trilex st{getInCode = True} ll
templateProcessor st ("syntax", ll) = (st, mainer)
  where mainer :: String
        mainer
          = "\\begin{TemplateCodeInside}{}{\\baselineskip}{\\baselineskip}{}{}{}\n"
              ++ code ++ "\n\\end{TemplateCodeInside}\n"
        
        code :: String
        code = trilex st{getInCode = True} ll
templateProcessor st ("HaskellGHCi", ll) = (st, mainer)
  where mainer :: String
        mainer
          = "\\begin{TemplateCodeInside}{}{\\baselineskip}{\\baselineskip}{}{}{}\n"
              ++ code ++ "\n\\end{TemplateCodeInside}\n"
        
        code :: String
        code = trilexgen st{getInCode = True} ll "1"
templateProcessor st ("Java://", ll) = (st, mainer)
  where mainer :: String
        mainer
          = "\\begin{TemplateCodeInside}{}{\\baselineskip}{\\baselineskip}{}{}{}\n"
              ++ code ++ "\n\\end{TemplateCodeInside}\n"
        
        code :: String
        code = trilexgen st{getInCode = True} ll "1"
templateProcessor st ("java://", ll) = (st, mainer)
  where mainer :: String
        mainer
          = "\\begin{TemplateCodeInside}{}{\\baselineskip}{\\baselineskip}{}{}{}\n"
              ++ code ++ "\n\\end{TemplateCodeInside}\n"
        
        code :: String
        code = trilexgen st{getInCode = True} ll "1"
templateProcessor st ("java", ll) = (st, mainer)
  where mainer :: String
        mainer
          = "\\begin{TemplateCodeInside}{}{\\baselineskip}{\\baselineskip}{}{}{}\n"
              ++ code ++ "\n\\end{TemplateCodeInside}\n"
        
        code :: String
        code = trilex st{getInCode = True} ll
templateProcessor st ("DOI", ll)
  = (st,
     "DOI:\\myhref{http://dx.doi.org/" ++ tl ++ "}{" ++ tx ++ "}")
  where tx = (treeToLaTeX (Map.findWithDefault [] "1" ll) st)
        tl = (shallowFlatten (Map.findWithDefault [] "1" ll))
templateProcessor st ("ISSN", ll)
  = (st,
     "\\myhref{http://dispatch.opac.d-nb.de/DB=1.1/CMD?ACT=SRCHA&IKT=8&TRM="
       ++ tl ++ "}{ISSN: " ++ tl ++ "}")
  where tl = (shallowFlatten (Map.findWithDefault [] "1" ll))
templateProcessor st ("cpp", ll)
  = (st,
     ("{\\ttfamily " ++
        (treeToLaTeX (breakLines3 96 (Map.findWithDefault [] "1" ll)) st)
          ++ "}"))
templateProcessor st ("Schach: Vorlage:Schachbrett", _)
  = (st{getC = ((getC st) + 1)},
     "\n\n\\parbox[t]{" ++
       mysize ++
         "\\linewidth}{\n\\begin{center}\n" ++
           "\\includegraphics[width=" ++
             mysize ++
               "\\linewidth,height=6.5in,keepaspectratio]{../images/chess" ++
                 n ++
                   ".pdf}\\end{center}\n" ++
                     "Stellung " ++ n ++ "}\\vspace{0.75cm}\n\n")
  where mysize = printf "%0.5f" (getF st)
        n = show (getC st)
templateProcessor st ("Farblegende", ll)
  = (st,
     if p then "\\legendColorBox" ++ col ++ "{" ++ param2 ++ "}\n" else
       "\\legendNamedColorBox{" ++ colname ++ "}{" ++ param2 ++ "}\n")
  where (p, colname, col)
          = colinfo (treeToLaTeX (Map.findWithDefault [] "1" ll) st)
        param2 = (treeToLaTeX (Map.findWithDefault [] "2" ll) st)
templateProcessor st ("Farbe", ll) = (st, defineall ++ inside)
  where (pred1, colname1, col1) = colinfo (param "1")
        (pred2, colname2, col2) = colinfo (param "2")
        param n = (treeToLaTeX (Map.findWithDefault [] n ll) st)
        trans = ((param "2") == "transparent")
        define p colname col
          = (if p then "\\definecolor{" ++ colname ++ "}{rgb}" ++ col else
               "")
        defineall
          = (define pred1 colname1 col1) ++ (define pred2 colname2 col2)
        inside
          = if trans then
              "\\textcolor{" ++ colname1 ++ "}{" ++ (param "3") ++ "}" else
              "\\fcolorbox{" ++
                colname2 ++
                  "}{" ++
                    colname2 ++
                      "}{\\textcolor{" ++ colname1 ++ "}{" ++ (param "3") ++ "}}"
templateProcessor st ("cite paper", ll)
  = (st,
     (if Map.member "editor" ll then
        (treeToLaTeX (Map.findWithDefault [] "editor" ll) st) else
        (treeToLaTeX (Map.findWithDefault [] "author" ll) st))
       ++
       (if Map.member "coauthor" ll then
          "; " ++ (treeToLaTeX (Map.findWithDefault [] "coauthor" ll) st)
          else "")
         ++
         " " ++
           (treeToLaTeX (Map.findWithDefault [] "date" ll) st) ++
             ". " ++
               (treeToLaTeX (Map.findWithDefault [] "title" ll) st) ++
                 "- " ++
                   (treeToLaTeX (Map.findWithDefault [] "publisher" ll) st) ++
                     ". pp.  " ++
                       (treeToLaTeX (Map.findWithDefault [] "pages" ll) st) ++ "\n")
templateProcessor st ("Cite book", ll)
  = (st,
     (if Map.member "editor" ll then
        (treeToLaTeX (Map.findWithDefault [] "editor" ll) st) else
        (treeToLaTeX (Map.findWithDefault [] "author" ll) st))
       ++
       " " ++
         (treeToLaTeX (Map.findWithDefault [] "title" ll) st) ++
           ". " ++
             (treeToLaTeX (Map.findWithDefault [] "publisher" ll) st) ++
               ", " ++
                 (treeToLaTeX (Map.findWithDefault [] "address" ll) st) ++
                   ", " ++
                     (treeToLaTeX (Map.findWithDefault [] "year" ll) st) ++ "\n")
templateProcessor st ("cite book", ll)
  = (st,
     (if Map.member "editor" ll then
        (treeToLaTeX (Map.findWithDefault [] "editor" ll) st) else
        (treeToLaTeX (Map.findWithDefault [] "author" ll) st))
       ++
       " " ++
         (treeToLaTeX (Map.findWithDefault [] "title" ll) st) ++
           ". " ++
             (treeToLaTeX (Map.findWithDefault [] "publisher" ll) st) ++
               ", " ++
                 (treeToLaTeX (Map.findWithDefault [] "address" ll) st) ++
                   ", " ++
                     (treeToLaTeX (Map.findWithDefault [] "year" ll) st) ++ "\n")
templateProcessor st ("Cite article", ll)
  = (tempProcAdapter $ citearticle ll) st
templateProcessor st ("cite journal", ll)
  = (tempProcAdapter $ citearticle ll) st
templateProcessor st ("Citation", ll)
  = (tempProcAdapter $ citearticle ll) st
templateProcessor st ("Literatur", ll)
  = (tempProcAdapter $ citearticle ll) st
templateProcessor st ("cite techreport", ll)
  = (tempProcAdapter $ citearticle ll) st
templateProcessor st ("citation", ll)
  = (tempProcAdapter $ citearticle ll) st
templateProcessor st ("Cite episode", ll)
  = (tempProcAdapter $ citearticle ll) st
templateProcessor st ("cite newsgroup", ll)
  = (tempProcAdapter $ citearticle ll) st
templateProcessor st ("Ada/95/cite AI", ll)
  = (tempProcAdapter $ citearticle ll) st
templateProcessor st ("Ada/Cite cla", ll)
  = (tempProcAdapter $ citearticle ll) st
templateProcessor st ("cite", ll)
  = (tempProcAdapter $ citearticle ll) st
templateProcessor st ("cite news", ll)
  = (tempProcAdapter $ citearticle ll) st
templateProcessor st ("Druckversionsnotiz", _) = (st, "")
templateProcessor st ("meta", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'm') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("Wikipedia", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'w') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("Wikipedia2", ll)
  = (st,
     (wikiLinkToLaTeX
        ((C 'w') : (C ':') : (Map.findWithDefault [] "1" ll))
        st)
       ++
       (wikiLinkToLaTeX
          ((C 'w') : (C ':') : (Map.findWithDefault [] "2" ll))
          st))
templateProcessor st ("GLSL Programming Unity SectionRef", ll)
  = (st,
     wikiLinkToLaTeX ((map (C) xx) ++ (Map.findWithDefault [] "1" ll))
       st)
  where xx
          = if
              shallowFlatten (Map.findWithDefault [] "1" ll) `elem`
                ["OpenGL ES 2.0 Pipeline", "Vertex Transformations",
                 "Vector and Matrix Operations", "Applying Matrix Transformations",
                 "Rasterization", "Per-Fragment Operations"]
              then "GLSL Programming/" else "GLSL Programming/Unity/"
templateProcessor st ("S", ll)
  = (st,
     wikiLinkToLaTeX
       (((C 's') : (C ':') : (Map.findWithDefault [] "1" ll)) ++
          [C '|'] ++ (Map.findWithDefault [] "2" ll))
       st)
templateProcessor st ("wiktionary", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'w') :
          (C 'i') :
            (C 'k') : (C 't') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("indent", ll) = (st, go)
  where go
          = case
              reads (shallowFlatten (Map.findWithDefault [] "1" ll)) ::
                [(Integer, String)]
              of
                [(n, _)] -> "\\newline{}" ++
                              (concat (genericReplicate n "{$\\text{ }$}"))
                _ -> "\\newline{}"
templateProcessor st ("wikipedia", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'w') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("Wikiversity", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'v') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("wikiquote", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'q') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("W", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'w') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("wp", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'w') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("B", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'b') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("Wikipedia-inline", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'w') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("Wiktionary", ll)
  = (st,
     wikiLinkToLaTeX
       ((map C "wiktionary") ++
          ((C ':') : (Map.findWithDefault [] "1" ll)))
       st)
templateProcessor st ("B3D:N2P/VTT1", ll) = (st2, r)
  where (r, st2)
          = runState
              (wikiImageToLaTeX
                 ((C 'F') :
                    (C 'i') :
                      (C 'l') :
                        (C 'e') :
                          (C ':') :
                            (Map.findWithDefault [] "image" ll) ++
                              [C '|'] ++
                                (Map.findWithDefault [] "imageWidth" ll) ++
                                  [C '|'] ++ (Map.findWithDefault [] "text3" ll)))
              st
templateProcessor st ("Template:B3D:N2P/VTT1", ll) = (st2, r)
  where (r, st2)
          = runState
              (wikiImageToLaTeX
                 ((C 'F') :
                    (C 'i') :
                      (C 'l') :
                        (C 'e') :
                          (C ':') :
                            (Map.findWithDefault [] "image" ll) ++
                              [C '|'] ++
                                (Map.findWithDefault [] "imageWidth" ll) ++
                                  [C '|'] ++ (Map.findWithDefault [] "text3" ll)))
              st
templateProcessor st ("Vorlage:Referenzen Zitat", ll) = (st, r)
  where r = (wikiLinkToLaTeX
               ((Map.findWithDefault [] "1" ll) ++
                  [C '|', C '['] ++ (Map.findWithDefault [] "2" ll) ++ [C ']'])
               st)
templateProcessor st ("Referenzen Zitat", ll) = (st, r)
  where r = (wikiLinkToLaTeX
               ((Map.findWithDefault [] "1" ll) ++
                  [C '|', C '['] ++ (Map.findWithDefault [] "2" ll) ++ [C ']'])
               st)
templateProcessor st ("wikipediapar", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'w') :
          (C ':') :
            (Map.findWithDefault [] (if Map.member "2" ll then "2" else "1")
               ll))
       st)
templateProcessor st ("Wikipediapar", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'w') :
          (C ':') :
            (Map.findWithDefault [] (if Map.member "2" ll then "2" else "1")
               ll))
       st)
templateProcessor st ("Kasten", ll)
  = (st,
     "\\LaTeXZeroBoxTemplate{" ++
       (treeToLaTeX
          (Map.findWithDefault (Map.findWithDefault [] "inhalt" ll) "1" ll)
          st)
         ++ "}")
templateProcessor st ("Druckversion Titeleintrag", ll)
  = (st,
     "\\pagebreak{}\\begin{longtable}{|p{0.3\\linewidth}|p{0.7\\linewidth}|}\\hline\n"
       ++
       "\\multicolumn{2}{|p{1.0\\linewidth}|}{\\bfseries Standardtiteleintrag}"
         ++
         (if (Map.member "URL" ll) then
            "\\\\ \\hline\n \\fetchurlcaption & \\url{" ++
              (treeToLaTeX (Map.findWithDefault [] "URL" ll) st) ++ "}"
            else "")
           ++
           (if (Map.member "Buch" ll) then
              "\\\\ \\hline\n \\bookcaption & \\url{http://de.wikibooks.org/wiki/"
                ++
                replace2 (treeToLaTeX (Map.findWithDefault [] "Buch" ll) st) " "
                  "_"
                  ++ "}"
              else "")
             ++
             (if (Map.member "Sachgruppen" ll) then
                "\\\\ \\hline\n \\functionalgroupcaption & " ++
                  (treeToLaTeX (Map.findWithDefault [] "Sachgruppen" ll) st)
                else "")
               ++
               (if (Map.member "WeitereThemen" ll) then
                  "\\\\ \\hline\n \\futhertopicscaption & " ++
                    (treeToLaTeX (Map.findWithDefault [] "WeitereThemen" ll) st)
                  else "")
                 ++
                 (if (Map.member "Hauptautoren" ll) then
                    "\\\\ \\hline\n \\mainauthorscaption  & " ++
                      (treeToLaTeX (Map.findWithDefault [] "Hauptautoren" ll) st)
                    else "")
                   ++
                   (if (Map.member "Betreuer" ll) then
                      "\\\\ \\hline\n \\projecttexniciancaption & " ++
                        (treeToLaTeX (Map.findWithDefault [] "Betreuer" ll) st)
                      else "")
                     ++
                     "\\\\ \\hline\n \\organizationscaptions & Wikibooks" ++
                       (if (Map.member "Erscheinungsdatum" ll) then
                          "\\\\ \\hline\n \\datecaption & " ++
                            (treeToLaTeX (Map.findWithDefault [] "Erscheinungsdatum" ll) st)
                          else "")
                         ++
                         (if (Map.member "Standardnummer" ll) then
                            "\\\\ \\hline\n \\standardcodecaption & " ++
                              (treeToLaTeX (Map.findWithDefault [] "Standardnummer" ll) st)
                            else "")
                           ++
                           (if (Map.member "Buch" ll) then
                              "\\\\ \\hline\n \\maintitlecaption & " ++
                                (treeToLaTeX (Map.findWithDefault [] "Buch" ll) st)
                              else "")
                             ++
                             (if (Map.member "Verleger" ll) then
                                "\\\\ \\hline\n \\publishercaption & " ++
                                  (treeToLaTeX (Map.findWithDefault [] "Verleger" ll) st)
                                else "")
                               ++
                               (if (Map.member "Verlagsort" ll) then
                                  "\\\\ \\hline\n \\publishercitycaption & " ++
                                    (treeToLaTeX (Map.findWithDefault [] "Verlagsort" ll) st)
                                  else "")
                                 ++
                                 (if (Map.member "Regal" ll) then
                                    "\\\\ \\hline\n \\shelfcaption & " ++
                                      (treeToLaTeX (Map.findWithDefault [] "Regal" ll) st)
                                    else "")
                                   ++
                                   (if (Map.member "Umfang" ll) then
                                      "\\\\ \\hline\n \\sizecaption & " ++
                                        (treeToLaTeX (Map.findWithDefault [] "Umfang" ll) st)
                                      else "")
                                     ++ "\\\\ \\hline \n\\end{longtable}\n")
templateProcessor st ("Druckversion Titelseite", ll)
  = (st{getTitle = s}, s)
  where s = (if (Map.member "Haupttitel" ll) then
               "\\mymaintitle{" ++
                 (treeToLaTeX (Map.findWithDefault [] "Haupttitel" ll)
                    st{getInCenter = True})
                   ++ "}"
               else "")
              ++
              (if (Map.member "Untertitel" ll) then
                 "\\mysubtitle{" ++
                   (treeToLaTeX (Map.findWithDefault [] "Untertitel" ll)
                      st{getInCenter = True})
                     ++ "}"
                 else "")
                ++
                (if (Map.member "Autor" ll) then
                   "\\myauthor{" ++
                     (treeToLaTeX (Map.findWithDefault [] "Autor" ll)
                        st{getInCenter = True})
                       ++ "}"
                   else "")
templateProcessor st ('j' : ('a' : ('v' : ('a' : (':' : xs)))), ll)
  = (tempProcAdapter $ javakeyword xs ll "java:") st
templateProcessor st ('J' : ('a' : ('v' : ('a' : (':' : xs)))), ll)
  = (tempProcAdapter $ javakeyword xs ll "Java:") st
templateProcessor st ("Haskell lib", ll)
  = (st, linkToLaTeX link st "")
  where param :: String -> Maybe [Anything Char]
        param name = Map.lookup name ll
        package
          = fromMaybe (map (C) "base") $ param "p" `mplus` param "package"
        version
          = fromMaybe (map (C) "4.1.0.0") $ param "v" `mplus` param "version"
        
        unnamed :: Integer -> [Anything Char]
        unnamed i = fromMaybe [] $ param (show i)
        unnPars = takeWhile (not . null) $ map unnamed [1 ..]
        location
          = (map (C) "http://hackage.haskell.org/packages/archive/") ++
              package ++
                [C '/'] ++
                  version ++
                    (map (C) "/doc/html/") ++
                      (intercalate [C '-'] unnPars) ++ (map (C) ".html")
        caption = intercalate [C '.'] unnPars
        link = location ++ [C ' '] ++ caption
templateProcessor st ("V", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'v') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("wikisource", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 's') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("M", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'm') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("C", ll)
  = (st,
     "\\myhref{http://commons.wikimedia.org/wiki/" ++
       loc ++ "}{" ++ cap ++ "}")
  where loc = (shallowFlatten (Map.findWithDefault [] "1" ll))
        
        cap :: String
        cap
          = if (Map.member "2" ll) then
              (treeToLaTeX (Map.findWithDefault [] "2" ll) st) else
              (treeToLaTeX (Map.findWithDefault [] "1" ll) st)
templateProcessor st ("commons", ll)
  = (st,
     "\\myhref{http://commons.wikimedia.org/wiki/" ++
       loc ++ "}{" ++ cap ++ "}")
  where loc = (shallowFlatten (Map.findWithDefault [] "1" ll))
        
        cap :: String
        cap
          = if (Map.member "2" ll) then
              (treeToLaTeX (Map.findWithDefault [] "2" ll) st) else
              (treeToLaTeX (Map.findWithDefault [] "1" ll) st)
templateProcessor st ("Commonscat", ll)
  = (st,
     "\\myhref{http://commons.wikimedia.org/wiki/Category:" ++
       loc ++ "}{" ++ cap ++ "}")
  where loc = (shallowFlatten (Map.findWithDefault [] "1" ll))
        
        cap :: String
        cap
          = if (Map.member "2" ll) then
              (treeToLaTeX (Map.findWithDefault [] "2" ll) st) else
              (treeToLaTeX (Map.findWithDefault [] "1" ll) st)
templateProcessor st ("Commons", ll)
  = (st,
     "\\myhref{http://commons.wikimedia.org/wiki/" ++
       loc ++ "}{" ++ cap ++ "}")
  where loc = (shallowFlatten (Map.findWithDefault [] "1" ll))
        
        cap :: String
        cap
          = if (Map.member "2" ll) then
              (treeToLaTeX (Map.findWithDefault [] "2" ll) st) else
              (treeToLaTeX (Map.findWithDefault [] "1" ll) st)
templateProcessor st ("b", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'b') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("Wikiquote", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 'q') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("Wikisource", ll)
  = (st,
     wikiLinkToLaTeX
       ((C 's') : (C ':') : (Map.findWithDefault [] "1" ll))
       st)
templateProcessor st ("Reaktion", ll)
  = (st, edukte ++ " $\\rightarrow$ " ++ produkte)
  where reput :: [String] -> [[Anything Char]] -> [[Anything Char]]
        reput (k : ks) out
          = if Map.member k ll then
              reput ks ((Map.findWithDefault [] k ll) : out) else reput ks out
        reput [] out = out
        
        myjoin :: [[Anything Char]] -> String -> String
        myjoin (x : xs) acu
          = if xs == [] then
              (if acu == "" then (treeToLaTeX x st) else
                 acu ++ "+" ++ (treeToLaTeX x st))
              else
              (if acu == "" then myjoin xs (treeToLaTeX x st) else
                 myjoin xs acu ++ "+" ++ (treeToLaTeX x st))
        myjoin [] acu = acu
        edukte
          = myjoin (reput ["Edukt", "Edukt1", "Edukt2", "Edukt3"] []) ""
        produkte
          = myjoin (reput ["Produkt", "Produkt1", "Produkt2", "Produkt3"] [])
              ""
templateProcessor st ("Visual Basic .NET: Vorlage:Code", ll)
  = templateProcessor st ("C++-Programmierung/ Vorlage:Code", ll)
templateProcessor st ("Regal:Programmierung: Vorlage:Code", ll)
  = (st, mainer)
  where mainer :: String
        mainer
          = "\\TemplateCode{" ++
              header ++
                "}{" ++
                  footer ++
                    "}{" ++
                      marker ++
                        "}{}{" ++ output ++ "}{" ++ lang ++ "}{" ++ code ++ "}{}{}{}"
        
        marker :: String
        marker
          = if Map.member "error" ll then "e" else
              if Map.member "valid" ll then "valid" else ""
        
        header :: String
        header
          = if Map.member "kopf" ll then
              (treeToLaTeX (Map.findWithDefault [] "kopf" ll)
                 st{getInCode = True})
              else ""
        
        lang :: String
        lang
          = if Map.member "lang" ll then
              (treeToLaTeX (Map.findWithDefault [] "lang" ll)
                 st{getInCode = True})
                ++ " Quelltext"
              else ""
        
        code :: String
        code = trilex st{getInCode = True} ll
        
        output :: String
        output
          = if Map.member "output" ll then
              (treeToLaTeX (killnbsp (Map.findWithDefault [] "output" ll))
                 st{getInCode = True})
              else ""
        
        footer :: String
        footer
          = if Map.member "fuss" ll then
              (treeToLaTeX (Map.findWithDefault [] "fuss" ll)
                 st{getInCode = True})
              else ""
        killnbsp ((Environment HtmlChar (Str "nbsp") _) : xs) = xs
        killnbsp x = x
templateProcessor st (x, ll)
  = (tempProcAdapter $ unknownTemplate x ll) st

{-DHUN| This function is currently nearly unused, it is essentially the same as templateProcessor. But with this way of writing it down you can find out for which templates handler functions are registered. DHUN-}

templateRegistry ::
                 [(String, Map String [Anything Char] -> Renderer String)]
templateRegistry
  = [("Regal:Programmierung: Vorlage:Code",
      \ ll ->
        let marker :: String
            marker
              = if Map.member "error" ll then "e" else
                  if Map.member "valid" ll then "valid" else ""
            killnbsp ((Environment HtmlChar (Str "nbsp") _) : xs) = xs
            killnbsp x = x
            withdef x f
              = if Map.member x ll then
                  (treeToLaTeX2 (f (Map.findWithDefault [] x ll))) else return ""
          in
          do st <- get
             put $ st{getInCode = True}
             code <- trilex2 ll
             footer <- withdef "fuss" id
             header <- withdef "kopf" id
             lang <- do x <- (withdef "lang" id)
                        return $ if x == "" then "" else x ++ " Quelltext"
             output <- withdef "output" killnbsp
             st2 <- get
             put $ st2{getInCode = False}
             return $
               "\\TemplateCode{" ++
                 header ++
                   "}{" ++
                     footer ++
                       "}{" ++
                         marker ++
                           "}{" ++ output ++ "}{" ++ lang ++ "}{" ++ code ++ "}{}{}{}")]

{-DHUN| processing of Java keywords for the English wikibook on Java, each Java keyword got its own template there DHUN-}

javakeyword ::
            [Char] -> Map String [Anything Char] -> [Char] -> Renderer String
javakeyword xs ll j
  = if (xs `elem` keywords) then
      state $ \ st -> ("{\\bfseries " ++ xs ++ "}", st) else
      unknownTemplate (j ++ xs) ll
  where keywords
          = (["private", "int", "return", "void", "new", "class",
              "interface", "String", "null", "Object", "byte", "char", "short",
              "long", "double", "boolean", "if", "true", "static", "public",
              "protected", "extends", "throw", "catch", "throws", "try",
              "abstract", "false", "else", "switch", "continue", "import",
              "final", "break", "implements", "finally", "while", "string",
              "float", "do", "for", "case", "default", "package", "this"])

{-DHUN| Handler for the unknown template. That is the ones that no handler was registered for DHUN-}

unknownTemplate ::
                String -> Map String [Anything Char] -> Renderer String
unknownTemplate xx ll2
  = do st <- get
       let x = trim xx
           ll = Map.fromList (trimall (Map.toList ll2))
           trimall ((a, b) : xs) = ((trim a), b) : (trimall xs)
           trimall [] = []
           tm = (templateMap st)
           step_ttl y = treeToLaTeX2 (Map.findWithDefault [] y ll)
           maybe_known_sf
             = do lis <- Map.lookup x tm
                  guard (not $ null lis)
                  let (latexname : transparams) = lis
                  guard (latexname /= "")
                  return $
                    do out <- mapM step_ttl transparams
                       return $
                         "\\" ++
                           latexname ++ "{" ++ (intercalate "}{" (map trim out)) ++ "}"
           unknown_sf
             = do uparams <- mapM step_ttl $ Map.keys ll
                  return $
                    "\n\nUNKNOWN TEMPLATE  \n" ++
                      (drop 1 . nullinit $ show x) ++
                        "\n\n" ++ "{" ++ (intercalate "}{" uparams) ++ "}" ++ "\n\n"
         in fromMaybe unknown_sf maybe_known_sf

{-DHUN| helper function to generate image numbers for image in image galleries (gallery tag in mediawiki) the fist input parameter is the start number, the second the end number. A list of all numbers between start and end is returned DHUN-}

generateGINsHelper :: Int -> Int -> [Int]
generateGINsHelper b e
  = if b == e then [] else b : (generateGINsHelper (b + 1) e)

{-DHUN| function to generate image numbers for image in image galleries (gallery tag in mediawiki). The renderer start before the start of the gallery is given as first parameter. The state of the renderer after the end of the gallery is given as second parameter. A list containing the numbers of the images in the gallery is returned DHUN-}

generateGalleryImageNumbers :: MyState -> MyState -> [Int]
generateGalleryImageNumbers oldst newst
  = generateGINsHelper (getJ oldst) (getJ newst)

{-DHUN| strips center tags of the parse tree keeping the data inside the center tags in the parse tree, so just flattens out the center tags DHUN-}

uncenter :: [Anything t] -> [Anything t]
uncenter ((Environment Tag (TagAttr "center" _) l) : xs)
  = l ++ (uncenter xs)
uncenter ((Environment e s l) : xs)
  = (Environment e s (uncenter l)) : (uncenter xs)
uncenter (x : xs) = x : (uncenter xs)
uncenter [] = []


{-DHUN| converts a parse tree to latex. Takes the parse tree as first parameter. Takes the current state of the renderer as second input parameter. Returns the latex representation of the tree as return value. This function should only be used internally in latex renderer since it does not generate the table of names references for the ref tags. DHUN-}

treeToLaTeX :: [Anything Char] -> MyState -> String
treeToLaTeX l states = fst $ runState (treeToLaTeX2 l) states

{-DHUN| converts a parse tree to latex. Takes the parse tree as first parameter. Takes the current state of the renderer as second input parameter. Returns a tuple. the first element is the latex representation of the tree. The second is the new state of the renders. Does one run before the actual run, in order to generate a table of names references for the ref tags in mediawiki. This function should be called by the main program after the parser. DHUN-}

treeToLaTeX3 :: [Anything Char] -> MyState -> (String, MyState)
treeToLaTeX3 l st = runState ttl2twice st
  where ttl2twice
          = do _ <- treeToLaTeX2 l
               b <- get
               put st{fndict = fndict b}
               treeToLaTeX2 l

findwd :: String -> Maybe Float
findwd ('w':'i':'d':'t':'h':':':xs) = case ((reads (takeWhile (`elem` "01234567890.") xs))::[(Float,String)]) of 
                                      [(_,_)] -> Nothing
                                      _ -> Nothing
findwd (_:xs) = findwd xs
findwd [] = Nothing
 
{-DHUN| converts a parse tree to latex. Takes the parse tree as first parameter. Takes the current state of the renderer as second input parameter. Returns the latex representation of the tree as Renderer String. So it actually takes the current state of the renderer as additional monadic input parameter and returns a possible modified version of it as additional monadic return parameter. This function should only be used internally in latex renderer since it does not generate the table of names references for the ref tags. DHUN-}

treeToLaTeX2 :: [Anything Char] -> Renderer String
treeToLaTeX2 ll
  = do x <- allinfo
       return $ concat x
  where allinfo :: Renderer [String]
        allinfo = mapM nodeToLaTeX (removeBr ll)
        
        walk :: String -> [Anything Char] -> String -> Renderer String
        walk prefix l postfix
          = do d <- treeToLaTeX2 l
               return $ prefix ++ d ++ postfix
        
        walktrim :: String -> [Anything Char] -> String -> Renderer String
        walktrim prefix l postfix
          = do st <- get
               put $ st{getInHeading = True}
               d <- treeToLaTeX2 l
               st2 <- get
               put $ st2{getInHeading = getInHeading st}
               return $ prefix ++ (trim d) ++ postfix
        
        walkbf :: [Anything Char] -> Renderer String
        walkbf l
          = do st <- get
               put $
                 st{lastFontChanged = True,
                    fontStack =
                      ((fromMaybe
                          FontStyle{stylebase = Normal, bold = True, italic = False}
                          (maybeHead (fontStack st))){bold = True}
                         : (fontStack st))}
               d <- treeToLaTeX2 l
               st2 <- get
               put $ st2{fontStack = drop 1 (fontStack st2)}
               return $ "{\\bfseries " ++ (trim d) ++ "}"
        
        walkit :: [Anything Char] -> Renderer String
        walkit l
          = do st <- get
               put $
                 st{lastFontChanged = True,
                    fontStack =
                      ((fromMaybe
                          FontStyle{stylebase = Normal, bold = False, italic = True}
                          (maybeHead (fontStack st))){italic = True}
                         : (fontStack st))}
               d <- treeToLaTeX2 l
               st2 <- get
               put $ st2{fontStack = drop 1 (fontStack st2)}
               return $ "{\\itshape " ++ (trim d) ++ "}"
        
        walktt :: [Anything Char] -> Renderer String
        walktt l
          = do st <- get
               put $
                 st{lastFontChanged = True,
                    fontStack =
                      ((fromMaybe
                          FontStyle{stylebase = Mono, bold = False, italic = False}
                          (maybeHead (fontStack st))){stylebase = Mono}
                         : (fontStack st))}
               d <- treeToLaTeX2 l
               st2 <- get
               put $ st2{fontStack = drop 1 (fontStack st2)}
               return $ "{\\ttfamily " ++ (trim d) ++ "}"
        
        walkfn :: String -> [Anything Char] -> String -> Renderer String
        walkfn prefix l postfix
          = do st <- get
               put $ st{getInFootnote = True}
               d <- treeToLaTeX2 l
               st2 <- get
               put $ st2{getInFootnote = (getInFootnote st)}
               return $ prefix ++ d ++ postfix
        
        nodeToLaTeX :: Anything Char -> Renderer String
        nodeToLaTeX (C c)
          = do st <- get
               case (fontStack st) of
                   (x : _) -> if (getFont x c) == (font st) then return (chartrans c)
                                else
                                do put
                                     st{font = (getFont x c),
                                        lastFontChanged = (lastFontChanged st) && (not (c == ' '))}
                                   return
                                     ((if ((lastFontChanged st) && (c == ' ')) then "{$\\text{ }$}"
                                         else "")
                                        ++
                                        (fontsetter (getFont x c)) ++
                                          (fontstyler x) ++ (chartrans c))
                   _ -> return (chartrans c)
        nodeToLaTeX (Environment ForbiddenTag (Str s) _)
          = return $ s >>= chartrans
        nodeToLaTeX (Environment Wikitable (Str s) l)
          = do st <- get
               put $ st{getInTab = (getInTab st) + 1}
               d <- tableToLaTeX l False s Nothing
               st2 <- get
               put $ st2{getInTab = (getInTab st)}
               return d
        nodeToLaTeX (Environment Wikitable (TagAttr _ a) l)
          = do st <- get
               put $ st{getInTab = (getInTab st) + 1}
               d <- tableToLaTeX (subTableCellCorrect l) (maybe False (\x->or (map ($ x) (map isInfixOf ["navbox", "infobox"]))) (Map.lookup "class" a))
                      (if
                         (Map.lookup "class" a) `elem`
                           (map Just ["prettytable", "wikitable"])
                         then "class=\"wikitable\"" else "") ((Map.lookup "style" a)>>= findwd)  
               st2 <- get
               put $ st2{getInTab = (getInTab st)}
               return d
        nodeToLaTeX (Environment Wikilink _ l)
          = do st <- get
               if getInHeading st then return $ wikiLinkCaption l st else
                 if (isImage (shallowFlatten l)) then wikiImageToLaTeX l else
                   return $ wikiLinkToLaTeX l st
        nodeToLaTeX (Environment Link (Str s) l)
          = do st <- get
               if getInHeading st then
                 return $ linkCaption l st s (getInFootnote st) else
                 return $ linkToLaTeX l st s
        nodeToLaTeX (Environment Link2 (Str s) l)
          = nodeToLaTeX (Environment Link (Str s) l)
        nodeToLaTeX (Environment ItemEnv (Str _) [Item _]) = return []
        nodeToLaTeX (Environment ItemEnv (Str s) l)
          = do st <- get
               d <- if s == ";" then do fulllist else
                      do ff <- treeToLaTeX2
                                 (if
                                    (s == ":") &&
                                      ([] ==
                                         [x | x <- l,
                                          not
                                            ((case x of
                                                  Environment Math _ _ -> True
                                                  Item _ -> True
                                                  _ -> False)
                                               || (x `elem` (map (C) "\n\t \r;,.")))])
                                    then
                                    (shallowEnlargeMath
                                       [x | x <- l, not (x `elem` (map (C) "\n\t \r;,.:!?"))])
                                    else l)
                         return [(ff, "")]
               if s /= ";" then
                 return $
                   "\n\\begin{" ++
                     (itemEnvironmentName s (getF st)) ++
                       "}" ++
                         (itemEnvironmentParameters s (getF st)) ++
                           (mmm2 d) ++
                             "\n\\end{" ++ (itemEnvironmentName s (getF st)) ++ "}\n"
                 else return $ (prolist d st)
          where mmm2 d
                  = case d of
                        (x : _) -> fst x
                        _ -> []
                texit v
                  = do prep <- treeToLaTeX2 . prepart $ v
                       post <- treeToLaTeX2 . postpart $ v
                       return (prep, post)
                
                fulllist :: Renderer [(String, String)]
                fulllist = mapM texit vv
                
                vv :: [[Anything Char]]
                vv = [x | x <- splitOn [Item ';'] l, x /= []]
                prepart v = takeWhile ((/=) (C ':')) v
                postpart v
                  = case dropWhile ((/=) (C ':')) v of
                        (_ : xs) -> xs
                        x -> x
                
                prolist :: [(String, String)] -> MyState -> String
                prolist lis st
                  = do (prd, pod) <- lis
                       if pod == [] then (singlepart prd st) else (doublepart prd pod st)
                doublepart pre po st
                  = "{\\bfseries" ++
                      "\n\\begin{" ++
                        (itemEnvironmentName s (getF st)) ++
                          "}" ++
                            (itemEnvironmentParameters s (getF st)) ++
                              pre ++
                                "\n\\end{" ++
                                  (itemEnvironmentName s (getF st)) ++
                                    "}\n" ++
                                      "}" ++
                                        "\n\\begin{" ++
                                          (itemEnvironmentName ":" (getF st)) ++
                                            "}" ++
                                              (itemEnvironmentParameters s (getF st)) ++
                                                (itemEnvironmentParameters s (getF st)) ++
                                                  (itemSeperator2 s) ++
                                                    po ++
                                                      "\n\\end{" ++
                                                        (itemEnvironmentName ":" (getF st)) ++ "}\n"
                singlepart pre st
                  = "{\\bfseries" ++
                      "\n\\begin{" ++
                        (itemEnvironmentName s (getF st)) ++
                          "}" ++
                            (itemEnvironmentParameters s (getF st)) ++
                              pre ++
                                "\n\\end{" ++ (itemEnvironmentName s (getF st)) ++ "}\n" ++ "}\n"
        nodeToLaTeX (Item c) = return $ "\n" ++ (itemSeperator c) ++ " "
        nodeToLaTeX (Environment Itemgroup _ l) = walk "" l ""
        nodeToLaTeX (Environment Wikiheading (Str s) l)
          = do st <- get
               if (getInTab st) > 0 then
                 walktrim ("{\\Large ") (uncenter l) ("}\n") else
                 walktrim ("\\" ++ (getsec s) ++ "{") (uncenter l)
                   ("}\n" ++ (getsecpost s))
        nodeToLaTeX (Environment Tag (TagAttr ('h' : (x : [])) _) l)
          = if x `elem` "123456" then
              case reads [x] of
                  [] -> walk "" l ""
                  ((y, _) : _) -> let s = replicate y '=' in
                                    walktrim ("\\" ++ (getsec s) ++ "{") (uncenter l)
                                      ("}\n" ++ (getsecpost s))
              else walk "" l ""
        nodeToLaTeX (Environment Bold _ l) = walkbf l
        nodeToLaTeX (Environment Italic _ l) = walkit l
        nodeToLaTeX (Environment Chapter _ l)
          = do d <- treeToLaTeX2 l
               return $ "\\chapter{" ++ (chapterTransform d) ++ "}\n\\myminitoc\n"
        nodeToLaTeX (Environment Tag (TagAttr "sup" _) l)
          = do st <- get
               walk ((fontsetter (font st)) ++ "\\textsuperscript{") l "}"
        nodeToLaTeX (Environment Tag (TagAttr "li" _) l)
          = walk "\\item{}" l ""
        nodeToLaTeX (Environment Tag (TagAttr "a" d) l)
          = do st <- get
               if getInHeading st then treeToLaTeX2 l else
                 case Map.lookup "href" d of
                     Just g -> return $
                                 linkToLaTeX
                                   ((map (C)
                                       (case g of
                                            '/' : ('/' : gx) -> "http://" ++ gx
                                            '/' : _ -> wikiUrlDataToString (urld st) g
                                            _ -> g))
                                      ++ [C ' '] ++ l)
                                   st
                                   ""
                     Nothing -> treeToLaTeX2 l
        nodeToLaTeX (Environment Tag (TagAttr "ol" _) l)
          = do st <- get
               walk
                 ("\n\\begin{" ++
                    (itemEnvironmentName "#" (getF st)) ++
                      "}" ++ (itemEnvironmentParameters "#" (getF st)))
                 l
                 ("\n\\end{" ++ (itemEnvironmentName "#" (getF st)) ++ "}\n")
        nodeToLaTeX (Environment Tag (TagAttr "dd" _) l)
          = do st <- get
               walk
                 ("\n\\begin{" ++
                    (itemEnvironmentName ":" (getF st)) ++
                      "}" ++ (itemEnvironmentParameters ":" (getF st)) ++ "\n\\item{}")
                 l
                 ("\n\\end{" ++ (itemEnvironmentName ":" (getF st)) ++ "}\n")
        nodeToLaTeX (Environment Tag (TagAttr "ul" _) l)
          = do st <- get
               walk
                 ("\n\\begin{" ++
                    (itemEnvironmentName "*" (getF st)) ++
                      "}" ++ (itemEnvironmentParameters "*" (getF st)))
                 l
                 ("\n\\end{" ++ (itemEnvironmentName "*" (getF st)) ++ "}\n")
        nodeToLaTeX (Environment Tag (TagAttr "dir" _) l)
          = do st <- get
               walk
                 ("\n\\begin{" ++
                    (itemEnvironmentName "*" (getF st)) ++
                      "}" ++ (itemEnvironmentParameters "*" (getF st)))
                 l
                 ("\n\\end{" ++ (itemEnvironmentName "*" (getF st)) ++ "}\n")
        nodeToLaTeX (Environment Tag (TagAttr "strong" _) l) = walkbf l
        nodeToLaTeX (Environment Tag (TagAttr "dfn" _) l) = walkit l
        nodeToLaTeX (Environment Tag (TagAttr "var" _) l) = walkit l
        nodeToLaTeX (Environment Tag (TagAttr "q" _) l) = walkit l
        nodeToLaTeX (Environment Tag (TagAttr "sub" _) l)
          = do st <- get
               walk ((fontsetter (font st)) ++ "\\textsubscript{") l "}"
        nodeToLaTeX (Environment Tag (TagAttr "cite" _) l)
          = walk "\\newline\n \\quad {\\scshape " l "}"
        nodeToLaTeX (Environment Sup (Str s) _)
          = return $ "{$^{\\textrm{\\scriptsize " ++ s ++ "}}$}"
        nodeToLaTeX (Environment Sub (Str s) _)
          = return $ "{$_{\\textrm{\\scriptsize " ++ s ++ "}}$}"
        nodeToLaTeX (Environment Tag (TagAttr "u" _) l)
          = walk "\\uline{" l "}"
        nodeToLaTeX (Environment Tag (TagAttr "p" _) l)
          = do st <- get
               if  (not (getInFootnote st)) && (not (getInCaption st)) && (not ((getInTab st) > 0)) then walk "" l "\n\n" else walk "" l ""

        nodeToLaTeX (Environment Tag (TagAttr "ins" _) l)
          = walk "\\uline{" l "}"
        nodeToLaTeX (Environment Tag (TagAttr "del" _) l)
          = walk "\\sout{" l "}"
        nodeToLaTeX (Environment Tag (TagAttr "strike" _) l)
          = walk "\\sout{" l "}"
        nodeToLaTeX (Environment Tag (TagAttr "b" _) l) = walkbf l
        nodeToLaTeX (Environment Tag (TagAttr "script" _) _)
          = walk "" [] ""
        nodeToLaTeX (Environment Tag (TagAttr "style" _) _) = walk "" [] ""
        nodeToLaTeX (Environment Tag (TagAttr "dt" _) l) = walkbf l
        nodeToLaTeX (Environment Tag (TagAttr "i" _) l) = walkit l
        nodeToLaTeX (Environment Tag (TagAttr "em" _) l) = walkit l
        nodeToLaTeX (Environment Tag (TagAttr "s" _) l)
          = walk "\\sout{" l "}"
        nodeToLaTeX (Environment Tag (TagAttr "small" _) l)
          = do st <- get
               if getInHeading st then walk "" l "" else walk "{\\small " l "}"
        nodeToLaTeX (Environment Tag (TagAttr "center" _) l)
          = do d <- treeToLaTeX2 (shallowEnlargeMath l)
               return $ "\n\\begin{center}\n" ++ d ++ "\n\\end{center}\n"
        nodeToLaTeX (Environment Tag (TagAttr "ref" a) l)
          = do st <- get
               case Map.lookup "name" a of
                   Just n -> case Map.lookup n (fndict st) of
                                 Just lll -> go st lll
                                 Nothing -> if not (and (map (`elem` (map (C) " \r\n\t")) l)) then
                                              do put st{fndict = Map.insert n l (fndict st)}
                                                 st2 <- get
                                                 go st2 l
                                              else go st l
                   Nothing -> go st l
          where go ss xx
                  = if getInFootnote ss then walk "\\^{}\\{" xx "\\}" else
                      walkfn "\\myfootnote{" xx "}"
        nodeToLaTeX (Environment Tag (TagAttr "includeonly" _) l)
          = walk "" l ""
        nodeToLaTeX (Environment Tag (TagAttr "blockquote" _) l)
          = walk "\\begin{myblockquote}\n\\item{}" l "\n\\end{myblockquote}"
        nodeToLaTeX (Environment Tag (TagAttr "code" _) l) = walktt l
        nodeToLaTeX (Environment Tag (TagAttr "kbd" _) l) = walktt l
        nodeToLaTeX (Environment Tag (TagAttr "samp" _) l) = walktt l
        nodeToLaTeX (Environment Tag (TagAttr "tt" _) l) = walktt l
        nodeToLaTeX (Environment Tag (TagAttr "big" _) l)
          = do st <- get
               if getInHeading st then walk "" l "" else walk "{\\large " l "}"
        nodeToLaTeX (Environment Tag (TagAttr "div" a) l)
          = let co
                  = case Map.lookup "style" a of
                        Nothing -> Nothing
                        Just x -> case splitOn "background-color:" x of
                                      (_ : (y2 : [])) -> case splitOn ";" y2 of
                                                             (z1 : _) -> case (trim z1) of
                                                                             ('#' : gs) -> let (p,
                                                                                                _,
                                                                                                col)
                                                                                                 = colinfo
                                                                                                     ('l'
                                                                                                        :
                                                                                                        'l'
                                                                                                          :
                                                                                                          gs)
                                                                                             in
                                                                                             Just $
                                                                                               if p
                                                                                                 then
                                                                                                 "\\definecolor{shadecolor}{rgb}"
                                                                                                   ++
                                                                                                   col
                                                                                                 else
                                                                                                 "\\colorlet{shadecolor}{"
                                                                                                   ++
                                                                                                   z1
                                                                                                     ++
                                                                                                     "}"
                                                                             _ -> Just $
                                                                                    "\\colorlet{shadecolor}{"
                                                                                      ++ z1 ++ "}"
                                                             _ -> Nothing
                                      _ -> Nothing
                beg
                  = case co of
                        Nothing -> ""
                        Just x -> "\\LaTeXShadedColorBoxTemplate{" ++ x ++ "}{"
                en
                  = case co of
                        Nothing -> ""
                        Just _ -> "}"
              in
              if (Map.member "class" a) then
                if (Map.findWithDefault [] "class" a) `elem` ["noprint", "topicon"]
                  then return $ beg ++ en else walk beg l en
                else walk beg l en
        nodeToLaTeX (Environment Tag (TagAttr "span" a) l)
          = if (Map.member "class" a) then
              if
                ((Map.findWithDefault [] "class" a) `elem`
                   ["noprint", "latitude", "longitude", "elevation"])
                  || ((Map.findWithDefault [] "id" a) `elem` ["coordinates"])
                then return "" else walk "" l ""
              else walk "" l ""
        nodeToLaTeX (Environment Tag (TagAttr "br" _) _)
          = do st <- get
               return $
                 if (getInCenter st) then "\\\\" else
                   if (getInTab st) > 0 then "\\newline{}" else
                     "$\\text{ }$\\newline{}\n"
        nodeToLaTeX (Environment Source (TagAttr _ a) l)
          = do let g = case reverse l of
                           [] -> []
                           (x : xs) -> if x == (C '\n') then reverse xs else l
               let xg
                     = case g of
                           (C '\n') : xs -> xs
                           _ -> g
               let f = shallowFlatten (map renormalize (breakLines3 linewidth xg))
               let glines = (Map.lookup "line" a) /= Nothing
               let spg = splitOn "\n" f
               let spgl = length (show (length spg))
               let lino = linenumbers spgl 1 (length spg)
               let newlines
                     = if glines then
                         intercalate "\n" (map (\ (k, v) -> k ++ " " ++ v) (zip lino spg))
                         else f
               d <- treeToLaTeX2
                      (breakLines3 linewidth (if glines then map C newlines else xg))
               return ("\\TemplateSource{" ++ (rtrim d) ++ "}\n")
        nodeToLaTeX (Environment Tag (TagAttr "font" a) l)
          = if Map.member "style" a then
              if
                ((Map.findWithDefault [] "style" a) == "text-decoration:overline")
                then walk "\\myoverline{" l "}" else walk "" l ""
              else walk "" l ""
        nodeToLaTeX (Environment Tag _ l) = walk "" l ""
        nodeToLaTeX (Environment Preformat (TagAttr _ _) l)
          = do d <- treeToLaTeX2 (breakLines3 linewidth l)
               return $ "\\TemplatePreformat{" ++ d ++ "}"
        nodeToLaTeX (Environment DhunUrl _ l)
          = do st <- get
               put st{currentUrl = shallowFlatten l}
               return ""
        nodeToLaTeX (Environment NoWiki _ l) = walk "" l ""
        nodeToLaTeX (Environment HDevLine _ l) = walk "" l ""
        nodeToLaTeX (Environment PageBreak _ _) = return "\\pagebreak "
        nodeToLaTeX (Quad) = return "$\\text{ }${}"
        nodeToLaTeX (Environment TableCap _ l) = walk "" l ""
        nodeToLaTeX (Tab)
          = return $ concat (take tabwidth (repeat "${\\text{ }}${}"))
        nodeToLaTeX (Environment SpaceIndent _ l)
          = if onlySpaces l then return "\n" else
              do st <- get
                 z <- treeToLaTeX2 l
                 put st
                 if (all (\ x -> x `elem` "\r\n\t ") z) then return "" else
                   do d <- treeToLaTeX2 (breakLines3 linewidth l)
                      return $ (preput st) ++ "\\TemplateSpaceIndent{" ++ d ++ "}\n"
          where preput i = if getInCode i then "" else "\\\\\n\n"
        nodeToLaTeX (Environment Math _ l) = return $ mathToLatex l
        nodeToLaTeX (Environment BigMath _ l)
          = return $
              "\\begin{equation*}" ++ (mathTransform l) ++ "\\end{equation*}"
        nodeToLaTeX (Environment Greek (Str s) _)
          = return $ "{\\mbox{$\\" ++ s ++ "$}}"
        nodeToLaTeX (Environment P302 (Str s) _)
          = return $ "\\^{" ++ s ++ "}"
        nodeToLaTeX (Environment HtmlChar (Str s) _)
          = return $ getHtmlChar s
        nodeToLaTeX (Environment NumHtml (Str s) _)
          = return $
              case reads s of
                  [] -> case
                          do z <- case s of
                                      ('x' : xs) -> Just xs
                                      ('X' : xs) -> Just xs
                                      _ -> Nothing
                             g <- unhex z
                             return g
                          of
                            Just x -> chartrans . chr . fromIntegral $ x
                            Nothing -> ("&#" ++ s ++ ";") >>= chartrans
                  (x : _) -> chartrans . chr . fst $ x
        nodeToLaTeX (Environment Gallery _ l)
          = do st <- get
               put st{getInGallery = True}
               d <- galleryToLatex l
               st2 <- get
               put $ (newst st2){getInGallery = (getInGallery st)}
               return d
          where midst i = i{getInGallery = False}
                gins i = generateGalleryImageNumbers i (midst i)
                newst i
                  = (midst i){getGalleryNumbers =
                                (getGalleryNumbers (midst i)) ++ (map toInteger (gins i))}
        nodeToLaTeX (Environment ImageMap _ l)
          = treeToLaTeX2 (imageMapClean l)
        nodeToLaTeX (Environment Reserved (Str "&middot;") _)
          = return "{\\mbox{$\\cdot$}}"
        nodeToLaTeX (Environment Template (Str s) l) = templateToLatex l s
        nodeToLaTeX (Environment Label (Str s) _)
          = return $ "\\label{" ++ s ++ "}"
        nodeToLaTeX _ = return []

{-DHUN| Unicode escaping for latex strings DHUN-}

doUnicode :: String -> String
doUnicode ('\206' : ('\178' : xs))
  = "\\ensuremath{\\beta}" ++ doUnicode xs
doUnicode (x : xs) = x : doUnicode xs
doUnicode [] = []
