{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | A module for mutable states used in the programm
module MyState where

import BaseFont
import Control.Monad.Trans.State (State)
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Data.Serialize
import GHC.Generics
import MediaWikiParseTree

-- | a type used as mutable state while processing a table. See documentation of
-- the TableHelper module
data TableState = TableState
  { -- | the row index of the current row in the table
    rowCounter :: Int,
    -- | The row index of the last row of the header, may be used to create the
    -- endhead command for the last line of the table header in the second run
    -- of the table rendering facility
    inputLastRowOfHeader :: Int,
    -- | This is used to calculate the row index of the last row of the header
    -- in a first run of the table rendering facility
    outputLastRowOfHeader :: Int,
    -- | This is used to calculate if a table has header columns in the first
    -- run of the table rendering facility.
    outputTableHasHeaderRows :: Bool,
    -- | This signal if the multirowmap of the last row was empty. The
    -- multirowmap is a facility to keep track of cells which span multiple
    -- rows. This is true if the last column process had no such cells in
    -- itself.
    lastRowHadEmptyMultiRowMap :: Bool,
    -- | This is true if the current row is the first row of the table. As each
    -- row it starts with a tr HTML tag or similar start maker in the wiki. But
    -- in LaTeX no such marker is possible in the first row, so the marker needs
    -- to be ignored in the first column, this is why this bit is here.
    isFirstRow :: Bool,
    -- | This true if the last cell was a header cell. In HTML this is denoted
    -- by a th tag and in wiki notation by the single or double exclamation
    -- mark.
    lastCellWasHeaderCell :: Bool,
    -- | This is true if the current cell is still part of the header of the
    -- table. The table header consists of the   adjacent cells denoted with th
    -- tags in HTML or single or double exclamation mark in wiki notation in the
    -- beginning of the table.
    stillInTableHeader :: Bool,
    -- | The columns index of the current column of the table
    currentColumn :: Int,
    -- | A map of currently active multi row cells. Its a map. the keys of the
    -- map are the column indices. The values of the map are tuples. The first
    -- element of each tuple is row multiplicity the second on is the column
    -- multiplicity. See `TableHelper.multiRowDictChangeStart` for details.
    multiRowMap :: Map Int (Int, Int),
    -- | the total number of columns in the table
    numberOfColumnsInTable :: Int,
    -- | True if the last cell spans multiple rows
    lastCellWasMultiRow :: Bool,
    -- | True if the horizontal and vertical rules of the table shall be drawn.
    -- See `TableHelper.seperatingLinesRequested` for details.
    seperatingLinesRequestedForTable :: Bool,
    -- | True if the current row contains cells denoted by th in HTML notation
    -- or single or double exclamation mark in wiki notation. Note that this
    -- alone is not enough to be part of the header of the table. Only the
    -- adjacent lines which contain at least one cell for which
    -- currentRowIsHeaderRow is true in the beginning of the table are
    -- considered header rows. currentRowIsHeaderRow is set to true th cell and
    -- reset to falls by any tr tag. So it is only guaranteed to be right at the
    -- end of the current row.
    currentRowIsHeaderRow :: Bool,
    -- | True if the last cell was the first cell of a row of the table.
    lastCellWasNotFirstCellOfRow :: Bool,
    -- | The list of width of the column in units of the line width. See
    -- `LatexRenderer.wdth3` for details.
    columnsWidthList :: [Float],
    -- | True if the last cell spans multiple columns
    lastCellWasMultiColumn :: Bool,
    -- | Used for the table column pre compliation with LaTeX to calculate the
    -- column widths in an automated manner. If it has the value Just i then
    -- only the content of the column with index i is returned as LaTeX output
    -- and all other columns are written to the LaTeX output without with empty
    -- content.
    activeColumn :: Maybe Int
  }

-- | see documentation of the makeLables function in WikiHelper module
data UrlState = UrlState
  { -- | The number of the current label.
    iUrlState :: Int,
    -- | The page name of the downloaded page currently being processed by the
    -- algorithm.
    sUrlState :: String,
    -- | A mapping from combined page and chapter, section etc. names (like
    -- urls) to their label numbers.
    mUrlState :: Map String String
  }
  deriving (Show, Eq, Read, Serialize, Generic)

-- | see initial value of the type `UrlState`
initialUrlState :: UrlState
initialUrlState =
  UrlState {iUrlState = 0, sUrlState = "", mUrlState = Map.empty}

-- | a type used as mutable state during the course of the LaTeXRederer
data MyState = MyState
  { -- | the list of image names from the wiki. This list is generated during the run of the LaTeX- or HTML- renderers.
    getImages :: [String],
    -- | the number of images in the whole document
    getJ :: Int,
    -- | The scale factor. Roughly the space available in the current column of
    -- the table. Is equal to one if currently not inside a table.
    getF :: Float,
    -- | The number of chess diagrams. Gets calculated during the LaTeX-
    -- renderer run.
    getC :: Int,
    -- | The nesting depth of tables. Is equal to zero if currently not in a
    -- table environment.
    getInTab :: Int,
    -- | True if currently in a gallery environment, false otherwise.
    getInGallery :: Bool,
    -- | True if currently in a footnote, false otherwise.
    getInFootnote :: Bool,
    -- | True if currently in a heading, false otherwise.
    getInHeading :: Bool,
    -- | True if currently inside a center environment, false otherwise.
    getInCenter :: Bool,
    -- | True if currently inside a source code environment
    getInCode :: Bool,
    -- | The main title of the document currently being processed. Generated
    -- from the template Druckversion Titelseite if found in the wiki code being
    -- processed, with template expansion set to internal or user defined
    -- mapping file.
    getTitle :: String,
    -- | User defined or internal mapping of MediaWiki templates to LaTeX
    -- commands. See `LatexRenderer.getUserTemplateMap` for details.
    templateMap :: Map String [String],
    -- | A mapping of links (URLs) to labels in the LaTeX document. This is an
    -- input parameter for `LatexRenderer.treeToLaTeX3`.
    -- `LatexRenderer.treeToLaTeX3` also takes the parse tree with the labels
    -- added as input parameter. See documentation on `WikiHelper.makeLables`
    -- for details.
    urls :: Map String String,
    -- | The URL of the main document currently being processed. It has the type
    -- `WikiUrlData`. See `WikiLinkHelper.analyseNetloc` for details.
    urld :: WikiUrlData,
    -- | Stores the image numbers of the images inside galleries. It gets
    -- generated during the run of `LatexRenderer.treeToLaTeX3`. This is needed
    -- so the dithering of the resolution of images inside galleries can be
    -- modified independent of that of normal images.
    getGalleryNumbers :: [Integer],
    -- | The lemma of the page currently being processed. In pages which include
    -- multiple sub pages this refers to the current sub page
    currentUrl :: String,
    -- | A map of the name attributes of the ref tags in the wiki to their
    -- content (from the starting ref tag until the closing of the same ref
    -- tag). This multiple references to the same content can occur an will be
    -- printed as footnotes as many times as ref tags with the same name occur.
    fndict :: Map String [Anything Char],
    -- | The list has the same length as the number of tables in the document.
    -- For each table it contains a sublist. Each sublist contains as many
    -- entries as there are columns in the respective table. Each element of the
    -- sublist is the LaTeX source of the respective table with only the
    -- respective column active and all other cells populated by the empty
    -- string. This is needed for the precompilation of the LaTeX tables in
    -- order to determine the optimal with of the columns.
    tablist :: [[String]],
    -- | A map. The keys are the table numbers the values are a second map. The
    -- keys of the second map are the column numbers of the respective table.
    -- The values of the second map are the maximum column width determined by
    -- the first run of LaTeX. These are needed in order to calculate the
    -- optimal column widths in the final LaTeX document. See also
    -- `LatexRender.wdth` for details.
    tabmap :: Map Int (Map Int Double),
    -- | A stack of FontStyle instances. The top is the currently active
    -- fontstyle and is used to determine the current font. See
    -- `BaseFont.FontStyle` for details.
    fontStack :: [FontStyle],
    -- | The currently active Font. See `BaseFont.Font` for details.
    font :: Font,
    -- | The language code of the wiki website currently being processed. i.e.
    -- de for German and en for English. Extracted from the HTML tag of the
    -- current website.
    langu :: Maybe String,
    -- | A map. The key is a hash code of a formula in wiki notation. The value
    -- is the width of the formula in pixels. This is used to create the
    -- embedding of the png files of the formulas when generating HTML code.
    forms :: Map String Int,
    -- | The character that was written to the output just before the current
    -- character
    lastChar :: Char,
    -- | True if the `BaseFont.FontStyle` has changed and no other character but
    -- (zero or more) space characters has been processed since then.
    lastFontChanged :: Bool,
    -- | The nesting level of captions of Wikilinks. Here image and other files
    -- are also processed as Wikilinnks with possible captions. Especially if an
    -- image has got a caption which contains a Wikilink with another caption
    -- this caption is processed with getCaptionLevel greater than 1.
    getCaptionLevel :: Integer,
    -- | True if the current run is configured to use vector graphics (pdf files
    -- converted from svg files) for images instead of png raster images
    -- converted from svg files
    vector :: Bool,
    -- | A list of HTML Strings. It contains all tables as well as all images
    -- that contain position markers which shall be converted to PDF by the
    -- chromium browser for inclusion as PDF files in the LaTeX document.
    htmlTables :: [String],
    -- | True if all tables shall be typeset with LaTeX and none with chromium.
    -- If it is false complex tables will be converted to PDF by the chromium
    -- browser and included by the LaTeX code as PDFs.
    latexTabs :: Bool,
    -- | A list HTML Strings. It contains all maps included in the wiki from the
    -- openstreetmap. They are included by the LaTeX code as PDF files which
    -- will be generated by the chromium browser
    htmlMaps :: [String],
    -- | True if currently processing data inside an space indented environment.
    -- That means that the current environment is written in the wiki by
    -- beginning every line of the environment with a space character. It is
    -- similar to the pre HTML tag but allows some wiki markup inside itself.
    isSpaceIndent :: Bool,
    -- | A map of footnotes for which the footnotenumer (footnotemark) has
    -- already been written to the LaTeX document, but for which the content of
    -- the footnote has not yet been written to the LaTeX document. This is
    -- because footnote inside tables or image caption can not be directly
    -- written to the LaTeX document. So that the footenote mark is written
    -- directly and the content of the footnote get written to the LaTeX
    -- document after the table of image environment in the LaTeX document has
    -- been closed.
    footnoteMap :: Map Integer String,
    -- | The current number of the footnote currently being processed. See also
    -- `footnoteMap`
    footnoteNumber :: Integer,
    -- | The HTML style tags in the HTML document downloaded from the server
    -- running MediaWiki parsed by the HTML parser. Theses are needed for
    -- rendering the HTML correctly with the chromium browser.
    styles :: [Anything Char],
    -- | The wiki source code of the chess boards in the wiki.
    chessBoards :: [String]
  }
  deriving (Show, Eq, Serialize, Generic)

-- | True if currently processing content inside a caption of an image of
-- wikilink.
getInCaption ::
  -- | The current state of the renderer
  MyState ->
  -- | True if currently inside a caption
  Bool
getInCaption st = (getCaptionLevel st) > 0

-- | Renderer is the State monad using MyState as mutable state
type Renderer = State MyState

-- | the initial value for MyState
initialState :: MyState
initialState =
  MyState
    { getImages = [],
      getJ = 1,
      getF = 1,
      getC = 1,
      getInTab = 0,
      getInGallery = False,
      getInFootnote = False,
      getInHeading = False,
      getInCenter = False,
      getInCode = False,
      getTitle = "",
      templateMap = Map.fromList [],
      urls = Map.empty,
      urld = BaseUrl (WikiBaseUrl ""),
      getGalleryNumbers = [],
      currentUrl = "",
      fndict = Map.empty,
      tablist = [],
      tabmap = Map.empty,
      fontStack =
        [FontStyle {stylebase = Normal, bold = False, italic = False}],
      font = ComputerModernRoman,
      langu = Nothing,
      forms = Map.empty,
      lastChar = ' ',
      lastFontChanged = False,
      getCaptionLevel = 0,
      vector = False,
      htmlTables = [],
      latexTabs = False,
      htmlMaps = [],
      isSpaceIndent = False,
      footnoteMap = Map.empty,
      footnoteNumber = 1,
      styles = [],
      chessBoards = []
    }

-- | represents an URL to a wiki (not to a page thereof), which is not a sister
-- project of wikipedia, so not wikibooks wikisource, etc.
data WikiBaseUrl = WikiBaseUrl
  { -- | the hostname of a wiki which is not an wikimedia wiki. So a hostname that is not wikibooks, wikipedia, etc.
    baseUrl :: String
  }
  deriving (Show, Eq, Serialize, Generic)

-- | represents an URL to a wiki (not to a page thereof), which is a sister
-- project of wikipedia, so wikibooks wikisource, etc.
data WikiUrlInfo = WikiUrlInfo
  { -- | The language code of wikimedia project (en for English or de for German)
    language :: String,
    -- | The type code of the wikimedia project (wikipedia, wikibooks,
    -- wikisource, etc.)
    wikitype :: String
  }
  deriving (Show, Eq, Serialize, Generic)

-- | represents an URL to a wiki (not to a page thereof), which is either a
-- sister project of wikipedia, so wikibooks wikisource, etc. or isn't a sister
-- project of wikipedia
data WikiUrlData
  = -- | A hostname to a non wikimedia wiki (see `WikiBaseUrl` for details)
    BaseUrl WikiBaseUrl
  | -- | A hostname of a wikimedia wiki (see `WikiUrlInfo` for details)
    UrlInfo WikiUrlInfo
  deriving (Show, Eq, Serialize, Generic)

-- | represents an URL to a page on a wiki
data WikiLinkInfo = WikiLinkInfo
  { -- | The host name of the wiki in a parsed form.
    urldata :: WikiUrlData,
    -- | The lemma (name of the article) on wiki currently being processed
    page :: String
  }
  deriving (Show, Eq, Serialize, Generic)
